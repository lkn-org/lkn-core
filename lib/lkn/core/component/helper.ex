defmodule TypedElement do
  @moduledoc false

  defstruct [
    :e,
    :t,
  ]

  def parse({:::, _, [elem, type]}) do
    %TypedElement{
      e: elem,
      t: type
    }
  end
end

defmodule Arg do
  @moduledoc false

  defstruct [
    :name,
    :type,
  ]

  def parse(line) do
    te = TypedElement.parse(line)
    %Arg{
      name: te.e,
      type: te.t,
    }
  end
end

defmodule Func do
  @moduledoc false

  defstruct [
    :name,
    :arguments,
  ]

  def parse({funcname, _, arglist}) do
    %Func{
      name: funcname,
      arguments: Enum.map(arglist, &(Arg.parse(&1))),
    }
  end
end

defmodule Cast do
  @moduledoc false

  defstruct [
    :doc,
    :fun,
  ]

  def parse(fun) do
    %Cast{
      doc: :none,
      fun: Func.parse(fun),
    }
  end
end

defmodule Call do
  @moduledoc false

  defstruct [
    :doc,
    :fun,
    :ret,
  ]

  def parse(fun) do
    te = TypedElement.parse(fun)
    %Call{
        doc: :none,
        fun: Func.parse(te.e),
        ret: te.t
     }
  end
end
