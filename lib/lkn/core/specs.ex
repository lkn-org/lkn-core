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
defmodule Lkn.Core.Specs do
  @moduledoc false

  def var_name(namestr) do
    {String.to_atom(namestr), [], nil}
  end

  defp check_specs(casts, calls, _legit, keywords) do
    allow_impl = Keyword.get(keywords, :allow_impl, false)
    allow_specs = Keyword.get(keywords, :allow_specs, true)

    Enum.map(casts ++ calls,
      &(case &1 do
          {_, _} -> if !allow_impl do raise "Cannot define an implementation here" end
          _ -> if !allow_specs do raise "Cannot require specs implementation here" end
        end))
  end

  def gen_server_plugin_entry_point(block, key_type, key_to_name, state_type) do
    block = case block do
              {:__block__, _, x} -> x
              x -> [x]
            end

    {casts, calls, legit} = parse_specs(block, [], [], [])

    check_specs(casts, calls, legit, [allow_impl: true, allow_specs: false])

    plugin = quote do :plugin end

    casts_client = Enum.map(casts, &(cast_client(plugin, var_name("key"), key_type, key_to_name, &1)))
    casts_behaviour = Enum.map(casts, &(cast_server(&1, var_name("key"), key_type, [], state_type, "_plugin")))

    quote do
      unquote(casts_client)
      unquote(casts_behaviour)
    end
  end

  def gen_server_from_specs(block, key_type, key_to_name, state_type, keywords \\ []) do
    block = case block do
              {:__block__, _, x} -> x
              x -> [x]
            end

    {casts, calls, legit} = parse_specs(block, [], [], [])

    check_specs(casts, calls, legit, keywords)

    impl_suffix = Keyword.get(keywords, :impl_suffix, "")
    key_name = Keyword.get(keywords, :key_name, var_name("key"))
    additional_args = Keyword.get(keywords, :additional_args, [])

    spec = quote do :spec end

    casts_client = Enum.map(casts, &(cast_client(spec, key_name, key_type, key_to_name, &1)))
    calls_client = Enum.map(calls, &(call_client(spec, key_name, key_type, key_to_name, &1)))

    casts_behaviour = Enum.map(casts, &(cast_server(&1, key_name, key_type, additional_args, state_type, impl_suffix)))
    calls_behaviour = Enum.map(calls, &(call_behaviour(&1, key_name, key_type, additional_args, state_type)))

    quote do
      unquote(legit)
      unquote(casts_client)
      unquote(calls_client)
      unquote(casts_behaviour)
      unquote(calls_behaviour)
    end
  end

  defp cast_client(namespace, key_name, key_type, key_to_name, cast) do
    cast = case cast do
             {cast, _} -> cast
             cast -> cast
           end

    name = cast.fun.name

    cast_doc = if cast.doc != :none do
      quote do
        @doc unquote(cast.doc)
      end
    end

    arglist = Enum.map(cast.fun.arguments, &(&1.name))
    arglistcl = [key_name|arglist]
    argtypes = [key_type|Enum.map(cast.fun.arguments, &(&1.type))]

    quote do
      unquote(cast_doc)
      @spec unquote({name, [], argtypes}) :: :ok
      def unquote({name, [], arglistcl}) do
        GenServer.cast(unquote(key_to_name).(unquote(key_name)),
                       {unquote(namespace), {unquote(name), unquote(arglist)}})
      end
    end
  end

  defp call_client(namespace, key_name, key_type, key_to_name, call) do
    name = call.fun.name

    call_doc = if call.doc != :none do
      quote do
        @doc unquote(call.doc)
      end
    end

    arglist = Enum.map(call.fun.arguments, &(&1.name))
    arglistcl = [key_name|arglist]
    argtypes = [key_type|Enum.map(call.fun.arguments, &(&1.type))]

    quote do
      unquote(call_doc)
      @spec unquote({name, [], argtypes}) :: unquote(call.ret)
      def unquote({name, [], arglistcl}) do
        GenServer.call(unquote(key_to_name).(unquote(key_name)),
                       {unquote(namespace), {unquote(name), unquote(arglist)}})
      end
    end
  end

  defp cast_server({cast, block}, key_name, key_type, additional_args, state_type, impl_suffix) do
    name = cast.fun.name
    name = String.to_atom(String.replace_suffix(Atom.to_string(name), "", impl_suffix))

    arglistcl_type = [{:::, [], [key_name, key_type]}
                      |Enum.map(cast.fun.arguments, &({:::, [], [&1.name, &1.type]}))] ++ additional_args ++ [
      {:::, [], [{:state, [], nil}, state_type]}
    ]
    arglistcl = [key_name
                 |Enum.map(cast.fun.arguments, &(&1.name))] ++
      Enum.map(additional_args, &(case &1 do
                                    {:::, [], [name, _]} -> name
                                  end)) ++ [{:state, [], nil}]

    quote do
      @doc false
      @spec unquote({name, [], arglistcl_type}) :: unquote(state_type)
      def unquote({name, [], arglistcl}) do
        unquote(block)
      end
    end
  end
  defp cast_server(cast, key_name, key_type, additional_args, state_type, impl_suffix) do
    name = cast.fun.name
    name = String.to_atom(String.replace_suffix(Atom.to_string(name), "", impl_suffix))

    arglistcl = [{:::, [], [key_name, key_type]}
                 |Enum.map(cast.fun.arguments, &({:::, [], [&1.name, &1.type]}))] ++ additional_args ++ [
      {:::, [], [{:state, [], nil}, state_type]}
    ]

    quote do
      @callback unquote({name, [], arglistcl}) :: unquote(state_type)
    end
  end

  defp call_behaviour(call, key_name, key_type, additional_args, state_type) do
    name = call.fun.name

    arglistcl = [{:::, [], [key_name, key_type]}
                 |Enum.map(call.fun.arguments, &({:::, [], [&1.name, &1.type]}))] ++ additional_args ++ [
      {:::, [], [{:state, [], nil}, state_type]}
    ]

    quote do
      @callback unquote({name, [], arglistcl}) :: {unquote(state_type), unquote(call.ret)}
    end
  end

  defp parse_specs(block, casts, calls, legit) do
    case block do
      [{:@, _, [{:doc, _, [docstring]}]},
       {:@, _, [{:cast, _, [fun]}]}
        |rest] ->
        cast = %Cast{Cast.parse(fun)|doc: docstring}
        parse_specs(rest, [cast|casts], calls, legit)
      [{:@, _, [{:doc, _, [docstring]}]},
       {:cast, _, [fun, [do: block]]}
        |rest] ->
        cast = %Cast{Cast.parse(fun)|doc: docstring}
        parse_specs(rest, [{cast, block}|casts], calls, legit)
      [{:@, _, [{:doc, _, [docstring]}]},
       {:@, _, [{:call, _, [fun]}]}
        |rest] ->
        call = %Call{Call.parse(fun)|doc: docstring}
        parse_specs(rest, casts, [call|calls], legit)
      [{:@, _, [{:cast, _, [fun]}]}
        |rest] ->
        cast = Cast.parse(fun)
        parse_specs(rest, [cast|casts], calls, legit)
      [{:cast, _, [fun, [do: block]]}
        |rest] ->
        cast = Cast.parse(fun)
        parse_specs(rest, [{cast, block}|casts], calls, legit)
      [{:@, _, [{:call, _, [fun]}]}
        |rest] ->
        call = Call.parse(fun)
        parse_specs(rest, casts, [call|calls], legit)
      [x|rest] ->
        parse_specs(rest, casts, calls, [x|legit])
      [] -> {casts, calls, Enum.reverse(legit)}
    end
  end
end
