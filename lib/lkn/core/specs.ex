defmodule Lkn.Core.Specs do
  @moduledoc false

  def gen_server_from_specs(block, key_type, key_to_name, state_type) do
    block = case block do
              {:__block__, _, x} -> x
              x -> [x]
            end

    {casts, calls, legit} = parse_specs(block, [], [], [])

    casts_client = Enum.map(casts, &(cast_client(key_type, key_to_name, &1)))
    calls_client = Enum.map(calls, &(call_client(key_type, key_to_name, &1)))

    casts_behaviour = Enum.map(casts, &(cast_behaviour(&1, state_type)))
    calls_behaviour = Enum.map(calls, &(call_behaviour(&1, state_type)))

    quote do
      unquote(legit)
      unquote(casts_client)
      unquote(calls_client)
      unquote(casts_behaviour)
      unquote(calls_behaviour)
    end
  end

  defp cast_client(key_type, key_to_name, cast) do
    name = cast.fun.name

    cast_doc = if cast.doc != :none do
      quote do
        @doc unquote(cast.doc)
      end
    end

    arglist = Enum.map(cast.fun.arguments, &(&1.name))
    arglistcl = [{:key, [], nil}|arglist]
    argtypes = [key_type|Enum.map(cast.fun.arguments, &(&1.type))]

    quote do
      unquote(cast_doc)
      @spec unquote({name, [], argtypes}) :: :ok
      def unquote({name, [], arglistcl}) do
        GenServer.cast(unquote(key_to_name).(unquote({:key, [], nil})),
                       {:spec, {unquote(name), unquote(arglist)}})
      end
    end
  end

  defp call_client(key_type, key_to_name, call) do
    name = call.fun.name

    call_doc = if call.doc != :none do
      quote do
        @doc unquote(call.doc)
      end
    end

    arglist = Enum.map(call.fun.arguments, &(&1.name))
    arglistcl = [{:key, [], nil}|arglist]
    argtypes = [key_type|Enum.map(call.fun.arguments, &(&1.type))]

    quote do
      unquote(call_doc)
      @spec unquote({name, [], argtypes}) :: unquote(call.ret)
      def unquote({name, [], arglistcl}) do
        GenServer.call(unquote(key_to_name).(unquote({:key, [], nil})),
                       {:spec, {unquote(name), unquote(arglist)}})
      end
    end
  end

  defp cast_behaviour(cast, state_type) do
    name = cast.fun.name
    entity_key_type = quote do
      Lkn.Core.Entity.k
    end

    arglistcl = [{:::, [], [{:entity_key, [], nil}, entity_key_type]}
                 |Enum.map(cast.fun.arguments, &({:::, [], [&1.name, &1.type]}))] ++ [
      {:::, [], [{:state, [], nil}, state_type]}
    ]

    quote do
      @callback unquote({name, [], arglistcl}) :: unquote(state_type)
    end
  end

  defp call_behaviour(call, state_type) do
    name = call.fun.name
    entity_key_type = quote do
      Lkn.Core.Entity.k
    end

    arglistcl = [{:::, [], [{:entity_key, [], nil}, entity_key_type]}
                 |Enum.map(call.fun.arguments, &({:::, [], [&1.name, &1.type]}))] ++ [
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
       {:@, _, [{:call, _, [fun]}]}
        |rest] ->
        call = %Call{Call.parse(fun)|doc: docstring}
        parse_specs(rest, casts, [call|calls], legit)
      [{:@, _, [{:cast, _, [fun]}]}
        |rest] ->
        cast = Cast.parse(fun)
        parse_specs(rest, [cast|casts], calls, legit)
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
