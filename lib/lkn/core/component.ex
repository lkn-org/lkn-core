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
defmodule Lkn.Core.Component do
  alias Lkn.Core.Specs

  @moduledoc """
  A behaviour module for implementing a Component which abstracts away
  an Entity.

  A Component is a Process which acts as a Proxy between an Entity
  (either a Puppet or a Map) and a System. It provides an unified
  interface the latter can use to modify the former. Thanks to this
  abstraction, the underlying structure of the Entity does not matter.

  The core of this module is the `defcomponent/2` macro. This macro
  has to be used to define a Component *specification*. A Component
  Specification is itself a behaviour module: it defines the Component
  interface concrete implementation will have to satisfy.

  Before defining a System (see `Lkn.Core.System` for more explanation
  on how this can be done, and more precisely
  `Lkn.Core.System.defsystem/2`), we need to define two
  Specifications. One for the Map and one for the Puppets. Here is an
  example.

      defcomponent Sys.Puppet do
        # 1. first, we need to specify the related System
        @system System.Chat

        # 2. We can define GenServer call-like functions which returns a
        #    result
        @call fun1(x :: number) :: boolean

        # 3: And we can define GenServer cast-like functions which are
        #    not blocking
        @cast fun2(y :: String.t, z :: any)
      end

  The resulting module is a behaviour module which also implements the
  `__using__` macro. The concrete implementation of this Component
  specification can be written as follows:

      defmodule Puppet.Sys do
        use Sys.Puppet

        def init_state(entity_key) od
          # ...
          {:ok, state}
        end

        def fun1(entity_key, x, state) do
          # ...
          {true, state}
        end

        def fun2(entity_key, y, z, state) do
          # ...
          state
        end
      end

  Do not hesitate to generate the documentation in order to see the
  look of the generated Behaviour.

  Basically, for a GenServer cast-like function, the handler takes two
  arguments in addition to the ones written in the prototype: before,
  the underlying Entity key, after the Component dynamic state. It
  returns the new state once its job is done. It is exactly the same
  thing for a call, expect it returns a tuple `{res, new_state}`.
  """

  @typedoc """
  The inner state of a Component.

  It can makes sense that a given Component carries a state. The main
  idea is to have a dynamic state which is not persistent across
  server restarts.
  """
  @type state :: any

  @doc """
  A hook which is called while a concrete Component process is
  created. It takes an Entity key and returns the initialized state.
  """
  @callback init_state(entity_key :: Lkn.Core.Entity.k) :: {:ok, state} | :error

  @doc """
  A macro to define a Component Specification to latter be implemented
  for each compatible Entity.
  """
  defmacro defcomponent(name, do: block) do
    state_type = quote do Lkn.Core.Component.state end
    key_type = quote do Lkn.Core.Entity.k end
    key_to_name = quote do
      &(Lkn.Core.Name.component(&1, unquote(name)))
    end

    quote do
      defmodule unquote(name) do
        unquote(Specs.gen_server_from_specs(
              block,
              key_type,
              key_to_name,
              state_type,
              key_name: Specs.var_name("entity_key"),
            ))

        @doc false
        def system do
          @system
        end

        defmacro __using__(_) do
          quote do
            defmodule State do
              defstruct [
                :entity_key,
                :state,
              ]

              @type t :: %State{
                entity_key: Lkn.Core.Entity.k,
                state: Lkn.Core.Component.state,
              }
            end

            @behaviour unquote(__MODULE__)
            @behaviour Lkn.Core.Component

            use GenServer

            alias Lkn.Core.Entity
            alias Lkn.Core.Name
            alias Lkn.Core.Properties

            def specs do
              unquote(__MODULE__)
            end

            @spec start_link(Entity.k) :: GenServer.on_start
            def start_link(entity_key) do
              GenServer.start_link(__MODULE__, entity_key, name: Name.component(entity_key, unquote(__MODULE__)))
            end

            def init(entity_key) do
              {:ok, s} = init_state(entity_key)
              {:ok, %State{entity_key: entity_key, state: s}}
            end

            @spec read(Entity.k, Properties.prop) :: Option.t(Properties.value)
            defp read(entity_key, p) do
              Properties.read(entity_key, p)
            end

            @spec write(Entity.k, Properties.prop, Properties.value) :: :ok
            def write(entity_key, p, v) do
              Properties.write(entity_key, p, v)
            end

            def handle_cast({:spec, {name, args}}, state) do
              s = :erlang.apply(__MODULE__, name, [state.entity_key|args] ++ [state.state])

              {:noreply, %State{state|state: s}}
            end

            def handle_call({:spec, {name, args}}, _call, state) do
              {res, s} = :erlang.apply(__MODULE__, name, [state.entity_key|args] ++ [state.state])

              {:reply, res, %State{state|state: s}}
            end
          end
        end
      end
    end
  end
end
