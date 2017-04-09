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
defmodule Lkn.Core.Test do
  use ExUnit.Case, async: false
  doctest Lkn.Core.System
  doctest Lkn.Core.Component
  doctest Lkn.Core.Entity

  use Lkn.Foundation
  alias Lkn.Core.Name
  require Lkn.Core.Component
  alias Lkn.Core.Component

  defmodule Test.System do
    defmodule Component do
      @callback level_up(key :: any) :: {:level_up, key :: Entity.t, old :: integer, new :: integer}
    end

    defmodule Map do
      @callback level_max :: integer
    end

    use Lkn.Core.System,
      state: :ok,
      puppet_component: Component,
      map_component: Map

    def init_state(_map_key, _comp) do
      :ok
    end

    def start_link(instance_key, map_key) do
      Lkn.Core.System.start_link(__MODULE__, instance_key, map_key)
    end

    def entity_enter(:ok, _entities, key) do
      Lkn.Core.System.notify({:enter, key})
      :ok
    end

    def entity_leave(:ok, _entities, key) do
      Lkn.Core.System.notify({:leave, key})
      :ok
    end

    def system_cast({:level_up, entity_key}, entities, state) do
      notif = comp(entities, entity_key).level_up(entity_key)
      Lkn.Core.System.notify(notif)
      state
    end

    def level_up(key, entity_key) do
      cast(key, {:level_up, entity_key})
    end
  end

  defmodule Test.Map.Component do
    Component.mapify for: Test.System

    def level_max, do: 7
  end

  defmodule Test.Map do
    use Lkn.Core.Entity, components: [Test.Map.Component]

    def start_link(key) do
      Lkn.Core.Entity.start_link(__MODULE__, key, [])
    end

    def init_properties([]) do
      %{:delay => 100, :limit => 2}
    end
  end

  defmodule Test.Entity.Component do
    Component.puppetify for: Test.System

    def level_up(entity_key) do
      Option.some(lvl) = read(entity_key, :level)
      write(entity_key, :level, lvl + 1)

      {:level_up, lvl, lvl + 1}
    end
  end

  defmodule Test.Entity do
    use Lkn.Core.Entity, components: [Test.Entity.Component]

    def start_link(key, args = [name: _name, level: _level]) do
      Lkn.Core.Entity.start_link(__MODULE__, key, args)
    end

    def init_properties(name: name, level: level) do
      %{:name => name, :level => level}
    end
  end

  defmodule Test.Ghost do
    use Lkn.Core.Entity, components: []

    def start_link(key) do
      Lkn.Core.Entity.start_link(__MODULE__, key, [])
    end

    def init_properties([]) do
      %{:name => "ghost", :level => 0}
    end
  end

  test "spawning map" do
    map_key = UUID.uuid4()

    {:ok, _} = Test.Map.start_link(map_key)
  end

  test "spawning map and engine" do
    map_key = UUID.uuid4()
    instance_key = UUID.uuid4()

    {:ok, _} = Test.Map.start_link(map_key)
    {:ok, _} = Lkn.Core.Instance.start_link(map_key, instance_key)
  end

  test "spawning entity" do
    entity_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4)
  end

  test "has component" do
    entity_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4)

    assert Lkn.Core.Entity.has_component?(entity_key, Test.System)
    assert !Lkn.Core.Entity.has_component?(entity_key, Test.NoSystem)
  end

  test "spawning entity and read level" do
    entity_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4)
    Option.some(4) = Lkn.Core.Entity.read(entity_key, :level)
  end

  test "spawning everything" do
    map_key = UUID.uuid4()

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Lkn.Core.Pool.register_puppeteer(map_key, :actor)
    entity_key = UUID.uuid4()
    ghost_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4)
    {:ok, _} = Test.Ghost.start_link(ghost_key)

    Registry.register(Lkn.Core.Notifier, Name.notify_group(instance_key), [])

    0 = Lkn.Core.System.population_size(instance_key, Test.System)
    Lkn.Core.Instance.register_entity(instance_key, entity_key)
    1 = Lkn.Core.System.population_size(instance_key, Test.System)

    receive do
      {:notify, {:enter, x}} -> assert x == entity_key
      after 100 -> assert false
    end

    Lkn.Core.Instance.register_entity(instance_key, ghost_key)
    1 = Lkn.Core.System.population_size(instance_key, Test.System)

    receive do
      _ -> assert false
      after 100 -> :ok
    end

    Lkn.Core.Instance.unregister_entity(instance_key, entity_key)
    0 = Lkn.Core.System.population_size(instance_key, Test.System)

    receive do
      {:notify, {:leave, x}} -> assert x == entity_key
      after 100 -> assert false
    end
  end

  test "level up" do
    map_key = UUID.uuid4()

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Lkn.Core.Pool.register_puppeteer(map_key, :actor)
    entity_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4)

    Registry.register(Lkn.Core.Notifier, Name.notify_group(instance_key), [])

    Lkn.Core.Instance.register_entity(instance_key, entity_key)

    Test.System.level_up(instance_key, entity_key)

    receive do
      {:notify, {:enter, x}} -> assert x == entity_key
      after 100 -> assert false
    end

    receive do
      {:notify, {:level_up, 4, 5}} -> :ok
      after 100 -> assert false
    end
  end

  test "register then unregister" do
    map_key = UUID.uuid4()

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Lkn.Core.Pool.register_puppeteer(map_key, :actor)

    Lkn.Core.Instance.unregister_puppeteer(instance_key, :actor)
    Process.sleep(100)
    instance_key2 = Lkn.Core.Pool.register_puppeteer(map_key, :actor)

    assert(instance_key2 == instance_key)

    Lkn.Core.Instance.unregister_puppeteer(instance_key, :actor)
    Process.sleep(110)
    instance_key2 = Lkn.Core.Pool.register_puppeteer(map_key, :actor)

    assert(instance_key2 != instance_key)
  end

  test "three registers" do
    map_key = UUID.uuid4()

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Lkn.Core.Pool.register_puppeteer(map_key, :actor1)
    instance_key2 = Lkn.Core.Pool.register_puppeteer(map_key, :actor2)

    assert(instance_key2 == instance_key)

    instance_key3 = Lkn.Core.Pool.register_puppeteer(map_key, :actor3)

    assert(instance_key3 != instance_key)
  end
end
