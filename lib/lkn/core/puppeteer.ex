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

  defmacro __using__(state: state_t) do
    quote do
      use Lkn.Prelude
      use GenServer

      alias Lkn.Core.Instance
      alias Lkn.Core.Entity

      @type state :: unquote(state_t)

      defmodule State do
        @moduledoc false

        defstruct [
          :puppeteer_key,
          :map_key,
          :instance_key,
          :state,
        ]

        @type t :: %State{
          puppeteer_key: Puppeteer.t,
          instance_key:  Lkn.Prelude.Option.t(Instance.t),
          map_key:       Lkn.Prelude.Option.t(Entity.t),
          state:         unquote(state_t),
        }

        @spec new(Puppeteer.t, unquote(state_t)) :: t
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

      def handle_call({:find_instance, map_key}, _from, state) do
        instance_key = Lkn.Core.Pool.register_puppeteer(map_key, state.puppeteer_key, __MODULE__)

        state = %State{state|instance_key: Lkn.Prelude.Option.some(instance_key)}

        {:reply, instance_key, state}
      end


      def handle_info(msg, state) do
        state = %State{state|state: handle_message(state.state, msg)}

        {:noreply, state}
      end

      @behaviour Lkn.Core.Puppeteer
    end
  end

  @spec start_link(m, k, init_args) :: GenServer.on_start
  def start_link(module, puppeteer_key, args) do
    GenServer.start_link(module, {puppeteer_key, args}, name: Name.puppeteer(puppeteer_key))
  end

  @callback handle_message(state, any) :: state
  @callback init_state(init_args) :: {:ok, state}|:error
  @callback leave_instance(state, Instance.t) :: state

  @spec leave_instance(k, Instance.t) :: :ok
  def leave_instance(puppeteer_key, instance_key) do
    GenServer.cast(Name.puppeteer(puppeteer_key), {:leave_instance, instance_key})
  end

  @spec find_instance(k, Entity.t) :: Instance.t
  def find_instance(puppeteer_key, map_key) do
    GenServer.call(Name.puppeteer(puppeteer_key), {:find_instance, map_key})
  end
end
