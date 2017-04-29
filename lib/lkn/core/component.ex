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
alias Lkn.Core.Specs

defmodule Lkn.Core.Component do
  defp cast_client(module_name, cast) do
    name = cast.fun.name
    entity_key_type = quote do
      Lkn.Core.Entity.k
    end

    cast_doc = if cast.doc != :none do
      quote do
        @doc unquote(cast.doc)
      end
    end

    arglist = Enum.map(cast.fun.arguments, &(&1.name))
    arglistcl = [{:entity_key, [], nil}|arglist]
    argtypes = [entity_key_type|Enum.map(cast.fun.arguments, &(&1.type))]

    quote do
      unquote(cast_doc)
      @spec unquote({name, [], argtypes}) :: :ok
      def unquote({name, [], arglistcl}) do
        GenServer.cast(Lkn.Core.Name.component(unquote({:entity_key, [], nil}), unquote(module_name)),
                       {:spec, {unquote(name), unquote(arglist)}})
      end
    end
  end

  defp call_client(module_name, call) do
    name = call.fun.name
    entity_key_type = quote do
      Lkn.Core.Entity.k
    end

    call_doc = if call.doc != :none do
      quote do
        @doc unquote(call.doc)
      end
    end

    arglist = Enum.map(call.fun.arguments, &(&1.name))
    arglistcl = [{:entity_key, [], nil}|arglist]
    argtypes = [entity_key_type|Enum.map(call.fun.arguments, &(&1.type))]

    quote do
      unquote(call_doc)
      @spec unquote({name, [], argtypes}) :: unquote(call.ret)
      def unquote({name, [], arglistcl}) do
        GenServer.call(Lkn.Core.Name.component(unquote({:entity_key, [], nil}), unquote(module_name)),
                       {:spec, {unquote(name), unquote(arglist)}})
      end
    end
  end

  defp cast_behaviour(cast) do
    name = cast.fun.name
    entity_key_type = quote do
      Lkn.Core.Entity.k
    end

    arglistcl = [{:::, [], [{:entity_key, [], nil}, entity_key_type]}
                 |Enum.map(cast.fun.arguments, &({:::, [], [&1.name, &1.type]}))]

    quote do
      @callback unquote({name, [], arglistcl}) :: :ok
    end
  end

  defp call_behaviour(call) do
    name = call.fun.name
    entity_key_type = quote do
      Lkn.Core.Entity.k
    end

    arglistcl = [{:::, [], [{:entity_key, [], nil}, entity_key_type]}
                 |Enum.map(call.fun.arguments, &({:::, [], [&1.name, &1.type]}))]

    quote do
      @callback unquote({name, [], arglistcl}) :: unquote(call.ret)
    end
  end

  defmacro defcomponent(name, do: block) do
    block = case block do
              {:__block__, _, x} -> x
              x -> [x]
            end

    {casts, calls, legit} = Specs.parse_specs(block, [], [], [])

    casts_client = Enum.map(casts, &(cast_client(name, &1)))
    calls_client = Enum.map(calls, &(call_client(name, &1)))

    casts_behaviour = Enum.map(casts, &(cast_behaviour(&1)))
    calls_behaviour = Enum.map(calls, &(call_behaviour(&1)))

    quote do
      defmodule unquote(name) do
        unquote(legit)
        unquote(casts_client)
        unquote(calls_client)
        unquote(casts_behaviour)
        unquote(calls_behaviour)

        def system do
          @system
        end

        defmacro __using__(_) do
          quote do
            @behaviour unquote(__MODULE__)

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

            @spec read(Entity.k, Properties.prop) :: Option.t(Properties.value)
            defp read(entity_key, p) do
              Properties.read(entity_key, p)
            end

            @spec write(Entity.k, Properties.prop, Properties.value) :: :ok
            def write(entity_key, p, v) do
              Properties.write(entity_key, p, v)
            end

            def handle_cast({:spec, {name, args}}, entity_key) do
              :erlang.apply(__MODULE__, name, [entity_key|args])

              {:noreply, entity_key}
            end

            def handle_call({:spec, {name, args}}, _call, entity_key) do
              res = :erlang.apply(__MODULE__, name, [entity_key|args])

              {:reply, res, entity_key}
            end
          end
        end
      end
    end
  end
end
