defmodule Lkn.Core.Puppet do
  @moduledoc """
  A specialized module to easily implement Map, one of the two kind of Entities
  used by lkn.

  This module provides the `defmap/2` macro, to easily define a module which
  implements the `Lkn.Core.Entity` behaviour module.

  Defining a new map is pretty easy, at it can be seen as some kind of key-value
  store. First, the `@components` annotation has to be used to define the set of
  components the map can be abstracted away with. Then, the
  `Lkn.Core.Entity.init_properties/1` behaviour function needs to be
  implemented.

  Finally, the `defmap/2` macro user needs to implement a `start_link`
  functions, that is only a proxy to `Lkn.Core.Entity.start_link/3`; the last
  argument being the arguments for `init_properties`.

  Here is an example of a proper use of `defmap/2`.

      defmap Example.Puppet do
        @components [Example.Puppet.Sys1Comp, Example.Puppet.Sys1Comp]

        def start_link(key, args) do
          Lkn.Core.Entity.start_link(__MODULE__, key, args)
        end

        def init_properties(args) do
          # ...
        end
      end
  """

  @typedoc """
  A key to identify and reach a Puppet.
  """
  @type k :: any

  @doc """
  An helper macro to define a Puppet module.

  The main idea of this macro is to hide the boilerplate required to define a
  Puppet. Its counterpart`Lkn.Core.Map.defpuppet/2` can be used to define a
  Map.
  """
  defmacro defpuppet(name, do: block) do
    lines = case block do
              {:__block__, _, x} -> x
              x -> [x]
            end

    quote do
      defmodule unquote(name) do
        @after_compile __MODULE__

        unquote(lines)

        def components do
          @components
        end

        use Lkn.Core.Entity, components: @components

        def __after_compile__(_env, _bytecode) do
          # check if the components are effectively valid
          Enum.map(components(), fn cx ->
            c = Macro.expand(cx, __MODULE__)
            gold = c.specs().system().component(:puppet)

            if gold != c.specs() do
              raise "Module #{inspect c} implements #{inspect c.specs()} which is not the puppet component of #{inspect c.specs().system()}"
            end
          end)
        end
      end
    end
  end
end
