#
# lkn.core: an entity-component-system (ecs) implementation for lyxan
# Copyright (C) 2017 Thomas Letan <contact@thomasletan.fr>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
defmodule Lkn.Core.Puppeteer do
  alias Lkn.Core.Instance
  alias Lkn.Core.Name
  alias Lkn.Core.Specs

  @typedoc """
  A key to identify and reach a given Puppeteer.
  """
  @type k :: any

  @typedoc """
  A module which implements the Puppeteer behaviour.
  """
  @type m :: module

  @type init_args :: any

  @type state :: any

  defmacro defpuppeteer(name, do: block) do
    state_type = quote do Lkn.Core.Puppeteer.state end
    key_type = quote do Lkn.Core.Puppeteer.k end
    key_to_name = &(Lkn.Core.Name.puppeteer(&1))

    quote do
      defmodule unquote(name) do
        unquote(Specs.gen_server_from_specs(
              block,
              key_type,
              key_to_name,
              state_type,
              key_name: Specs.var_name("puppeteer_key"),
              additional_args: [
                quote do unquote(Specs.var_name("instance_key")) :: Lkn.Core.Instance.k end,
              ]
            )
        )

        defmacro __using__(args) do

          plugin_clients = case args do
                             [do: use_block] ->
                               Specs.gen_server_plugin_entry_point(
                                 use_block,
                                 "puppeteer_key",
                                 quote do Lkn.Core.Puppeteer.k end,
                                 &(Lkn.Core.Name.puppeteer(&1)),
                                 [quote do unquote(Specs.var_name("instance_key")) :: Lkn.Core.Instance.k end],
                                 quote do Lkn.Core.Puppeteer.state end
                               )
                             _ ->
                               quote do
                               end
                           end

          quote do
            @behaviour unquote(__MODULE__)
            @behaviour Lkn.Core.Puppeteer

            use GenServer

            alias Lkn.Core.Instance
            alias Lkn.Core, as: L

            unquote(Specs.gen_server_returns())

            defmodule PrivateState do
              @moduledoc false

              defstruct [
                :puppeteer_key,
                :map_key,
                :instance_key,
                :state,
              ]

              @type t :: %PrivateState{
                puppeteer_key: Puppeteer.k,
                instance_key:  Lkn.Prelude.Option.t(Instance.k),
                map_key:       Lkn.Prelude.Option.t(L.Map.k),
                state:         Lkn.Core.Puppeteer.state,
              }

              @spec new(Puppeteer.t, Lkn.Core.Puppeteer.state) :: t
              def new(pk, s) do
                %PrivateState{
                  puppeteer_key: pk,
                  instance_key:  Lkn.Prelude.Option.nothing(),
                  map_key:       Lkn.Prelude.Option.nothing(),
                  state:         s,
                }
              end

              def update(state, opts) do
                st = Keyword.get(opts, :state, state.state)
                instance = Keyword.get(opts, :instance, state.instance_key)
                mapk = Keyword.get(opts, :map, state.map_key)

                %PrivateState{state|
                       state: st,
                       map_key: mapk,
                       instance_key: instance
                }
              end
            end

            def init({puppeteer_key, args}) do
              {:ok, s} = init_state(args)
              {:ok, PrivateState.new(puppeteer_key, s)}
            end

            def handle_cast({:puppet_enter, instance_key, puppet_key, digest}, state) do
              if state.instance_key == Lkn.Prelude.Option.some(instance_key) do
                opts = puppet_enter(state.state, state.instance_key, puppet_key, digest)

                {:noreply, PrivateState.update(state, opts)}
              else
                {:noreply, state}
              end
            end
            def handle_cast({:puppet_leave, instance_key, puppet_key}, state) do
              if state.instance_key == Lkn.Prelude.Option.some(instance_key) do
                opts = puppet_leave(state.state, state.instance_key, puppet_key)

                {:noreply, PrivateState.update(state, opts)}
              else
                {:noreply, state}
              end
            end
            def handle_cast({:instance_digest, instance_key, map_key, map, puppets}, state) do
              if state.instance_key == Lkn.Prelude.Option.some(instance_key) do
                opts = instance_digest(state.state, instance_key, map_key, map, puppets)
                {:noreply, PrivateState.update(state, opts)}
              else
                {:noreply, state}
              end
            end
            def handle_cast({:leave_instance, instance_key}, state) do
              if state.instance_key == Lkn.Prelude.Option.some(instance_key) do
                opts = leave_instance(state.state, instance_key)

                Instance.unregister_puppeteer(instance_key, state.puppeteer_key)

                {:noreply, PrivateState.update(state, opts)}
              else
                {:noreply, state}
              end
            end
            def handle_cast({:spec, {name, args}}, state) do
              opts = :erlang.apply(__MODULE__, name, [state.puppeteer_key|args]++[state.instance_key, state.state])

              {:noreply, PrivateState.update(state, opts)}
            end
            def handle_cast({:plugin, {name, args}}, state) do
              name = String.to_atom(String.replace_suffix(Atom.to_string(name), "", "_plugin"))
              opts = :erlang.apply(__MODULE__, name, [state.puppeteer_key|args]++[state.instance_key, state.state])

              {:noreply, PrivateState.update(state, opts)}
            end

            def handle_call({:find_instance, map_key}, _from, state) do
              instance_key = Lkn.Core.Pool.register_puppeteer(map_key, state.puppeteer_key, __MODULE__)

              state = %PrivateState{state|instance_key: Lkn.Prelude.Option.some(instance_key)}

              {:reply, instance_key, state}
            end
            def handle_call({:spec, {name, args}}, _call, state) do
              {res, opts} = :erlang.apply(__MODULE__, name, [state.puppeteer_key|args] ++ [state.state])

              {:reply, res, PrivateState.update(state, opts)}
            end

            unquote(plugin_clients)

            def terminate(reason, state) do
              destroy(state.puppeteer_key, state.state, state.instance_key, reason)
            end
          end
        end
      end
    end
  end

  @spec start_link(m, k, init_args) :: GenServer.on_start
  def start_link(module, puppeteer_key, args) do
    GenServer.start_link(module, {puppeteer_key, args}, name: Name.puppeteer(puppeteer_key))
  end

  @callback init_state(init_args) :: {:ok, state}|:error
  @callback leave_instance(s :: state, instance_key :: Lkn.Core.Instance.k) :: state
  @callback puppet_enter(s :: state, instance_key :: Lkn.Core.Instance.k, puppet_key :: Lkn.Core.Puppet.k, digest :: Lkn.Core.Entity.digest) :: state
  @callback puppet_leave(s :: state, instance_key :: Lkn.Core.Instance.k, puppet_key :: Lkn.Core.Puppet.k) :: state
  @callback destroy(puppeteer_key :: k, s :: state, instance_key :: Option.t(Lkn.Core.Instance.k), reason :: any) :: term
  @callback instance_digest(
    s :: state,
    instance_key :: Lkn.Core.Instance,
    map_key :: Lkn.Core.Map.k,
    map :: Lkn.Core.Entity.digest,
    puppets :: %{Lkn.Core.Puppet.k => Lkn.Core.Entity.digest}
  ) :: state

  @spec leave_instance(k, Instance.k) :: :ok
  def leave_instance(puppeteer_key, instance_key) do
    GenServer.cast(Name.puppeteer(puppeteer_key), {:leave_instance, instance_key})
  end

  @spec find_instance(k, L.Map.k) :: Instance.k
  def find_instance(puppeteer_key, map_key) do
    GenServer.call(Name.puppeteer(puppeteer_key), {:find_instance, map_key})
  end

  @doc false
  def instance_digest(puppeteer_key, instance_key, map_key, map, puppets) do
    GenServer.cast(Name.puppeteer(puppeteer_key), {:instance_digest, instance_key, map_key, map, puppets})
  end

  @doc false
  def puppet_enter(puppeteer_key, instance_key, puppet_key, digest) do
    GenServer.cast(Name.puppeteer(puppeteer_key), {:puppet_enter, instance_key, puppet_key, digest})
  end

  @doc false
  def puppet_leave(puppeteer_key, instance_key, puppet_key) do
    GenServer.cast(Name.puppeteer(puppeteer_key), {:puppet_leave, instance_key, puppet_key})
  end

  def stop(puppeteer_key, reason \\ :normal) do
    GenServer.stop(Name.puppeteer(puppeteer_key), reason)
  end
end
