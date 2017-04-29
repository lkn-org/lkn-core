defmodule Lkn.Core.Specs do
  @moduledoc false

  def parse_specs(block, casts, calls, legit) do
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
