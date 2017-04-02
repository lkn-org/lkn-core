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
  @moduledoc """
  A behaviour module for implementing an `Lkn.Entity` Component.
  """

  alias Lkn.Core.Entity
  alias Lkn.Core.Name
  alias Lkn.Core.Properties

  defmacro __using__(sys: sys, type: type) do
    quote location: :keep do
      use GenServer
      use Lkn.Foundation

      @behaviour unquote(sys).component(unquote(type))

      @spec start_link(Entity.t) :: GenServer.on_start
      def start_link(entity_key) do
        Lkn.Core.Component.start_link(__MODULE__, entity_key, unquote(sys))
      end

      def system do
        unquote(sys)
      end

      @spec read(Entity.t, Properties.prop) :: Option.t(Properties.value)
      def read(entity_key, p) do
        GenServer.call(Name.component(entity_key, unquote(sys)), {:read, p})
      end

      @spec write(Entity.t, Properties.prop, Properties.value) :: :ok
      def write(entity_key, p, v) do
        GenServer.call(Name.component(entity_key, unquote(sys)), {:write, p, v})
      end

      def handle_call({:read, p}, _from, entity_key) do
        {:reply, Properties.read(entity_key, p), entity_key}
      end

      def handle_call({:write, p, v}, _from, entity_key) do
        {:reply, Properties.write(entity_key, p, v), entity_key}
      end
    end
  end

  defmacro puppetify(for: sys) do
    quote do
      use unquote(__MODULE__), sys: unquote(sys), type: :puppet
    end
  end

  defmacro mapify(for: sys) do
    quote do
      use unquote(__MODULE__), sys: unquote(sys), type: :map
    end
  end

  @spec start_link(module, Entity.t, module) :: GenServer.on_start
  def start_link(module, entity_key, sys) do
    GenServer.start_link(module, entity_key, name: Name.component(entity_key, sys))
  end
end
