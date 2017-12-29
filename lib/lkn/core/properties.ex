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
defmodule Lkn.Core.Properties do
  use Lkn.Prelude

  @moduledoc false

  @spec start_link(map, Lkn.Core.Entity.k) :: Agent.on_start
  def start_link(content, entity_key) do
    Agent.start_link(fn -> content end, name: Lkn.Core.Name.properties(entity_key))
  end

  @spec read(Lkn.Core.Entity.k, Lkn.Core.Entity.prop) :: Option.t(Lkn.Core.Entity.value)
  def read(entity_key, prop) do
    case Agent.get(Lkn.Core.Name.properties(entity_key), &Map.fetch(&1, prop)) do
      {:ok, val} -> Option.some(val)
      :error -> Option.nothing()
    end
  end

  @spec write(Lkn.Core.Entity.k, Lkn.Core.Entity.prop, Lkn.Core.Entity.value) :: :ok
  def write(entity_key, prop, v) do
    Agent.update(Lkn.Core.Name.properties(entity_key), &Map.put(&1, prop, v))
  end

  @spec compute(Lkn.Core.Entity.k, (%{Lkn.Core.Entity.prop => Lkn.Core.Entity.value} -> any)) :: any
  def compute(entity_key, function) do
    Agent.get(Lkn.Core.Name.properties(entity_key), function)
  end
end
