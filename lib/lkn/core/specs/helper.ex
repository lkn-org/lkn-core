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
