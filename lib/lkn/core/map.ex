defmodule Lkn.Core.Map do
  @typedoc """
  A key to identify and reach a Map.
  """
  @type k :: any

  defp parse_def(lines, comps, legit) do
    case lines do
      [{:@, _, [{:components, _, [cps]}]}
       |rest] ->
        parse_def rest, cps, legit
      [x|rest] ->
        parse_def rest, comps, [x|legit]
      [] ->
        {comps, Enum.reverse(legit)}
    end
  end

  defmacro defmap(name, do: block) do
    lines = case block do
              {:__block__, _, x} -> x
              x -> [x]
            end

    {comps, legit} = parse_def(lines, [], [])

    quote do
      defmodule unquote(name) do
        @after_compile __MODULE__

        use Lkn.Core.Entity, components: unquote(comps)

        unquote(legit)

        def __after_compile__(_env, _bytecode) do
          # check if the components are effectively valid
          Enum.map(unquote(comps), fn cx ->
            c = Macro.expand(cx, __MODULE__)
            gold = c.specs().system().component(:map)

            if gold != c.specs() do
              raise "Module #{inspect c} implements #{inspect c.specs()} which is not the map component of #{inspect c.specs().system()}"
            end
          end)
        end
      end
    end
  end
end
