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
        unquote(Specs.gen_server_from_specs(block, key_type, key_to_name, state_type))

        defmacro __using__(_) do
          quote do
            @behaviour unquote(__MODULE__)
            @behaviour Lkn.Core.Puppeteer

            use GenServer

            alias Lkn.Core.Instance
            alias Lkn.Core, as: L

            defmodule State do
              @moduledoc false

              defstruct [
                :puppeteer_key,
                :map_key,
                :instance_key,
                :state,
              ]

              @type t :: %State{
                puppeteer_key: Puppeteer.k,
                instance_key:  Lkn.Prelude.Option.t(Instance.k),
                map_key:       Lkn.Prelude.Option.t(L.Map.k),
                state:         Lkn.Core.Puppeteer.state,
              }

              @spec new(Puppeteer.t, Lkn.Core.Puppeteer.state) :: t
              def new(pk, s) do
                %State{
                  puppeteer_key: pk,
                  instance_key:  Lkn.Prelude.Option.nothing(),
                  map_key:       Lkn.Prelude.Option.nothing(),
                  state:         s,
                }
              end
            end

            def init({puppeteer_key, args}) do
              {:ok, s} = init_state(args)
              {:ok, State.new(puppeteer_key, s)}
            end

            def handle_cast({:leave_instance, instance_key}, state) do
              if state.instance_key == Lkn.Prelude.Option.some(instance_key) do
                s2 = leave_instance(state.state, instance_key)

                Instance.unregister_puppeteer(instance_key, state.puppeteer_key)

                {:noreply, %State{state|instance_key: Lkn.Prelude.Option.nothing(), state: s2}}
              else
                {:noreply, state}
              end
            end
            def handle_cast({:spec, {name, args}}, state) do
              s = :erlang.apply(__MODULE__, name, [state.puppeteer_key|args]++[state.state])

              {:noreply, %State{state|state: s}}
            end

            def handle_call({:find_instance, map_key}, _from, state) do
              instance_key = Lkn.Core.Pool.register_puppeteer(map_key, state.puppeteer_key, __MODULE__)

              state = %State{state|instance_key: Lkn.Prelude.Option.some(instance_key)}

              {:reply, instance_key, state}
            end
            def handle_call({:spec, {name, args}}, _call, state) do
              {res, s} = :erlang.apply(__MODULE__, name, [state.puppeteer_key|args] ++ [state.state])

              {:reply, res, %State{state|state: s}}
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
  @callback leave_instance(state, Instance.k) :: state

  @spec leave_instance(k, Instance.k) :: :ok
  def leave_instance(puppeteer_key, instance_key) do
    GenServer.cast(Name.puppeteer(puppeteer_key), {:leave_instance, instance_key})
  end

  @spec find_instance(k, L.Map.k) :: Instance.k
  def find_instance(puppeteer_key, map_key) do
    GenServer.call(Name.puppeteer(puppeteer_key), {:find_instance, map_key})
  end
end
