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

use Lkn.Prelude

alias Lkn.Core.Instance
alias Lkn.Core.Puppeteer

require Lkn.Core.Component

defmodule Lkn.Core.Test do
  use ExUnit.Case, async: false

  setup_all do
    Application.stop(:core)
    Application.start(:core)
    :ok
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

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4, owner: self())
  end

  test "spawning then stoping entity" do
    entity_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4, owner: self())
    Lkn.Core.Entity.stop(entity_key)

    receive do
      {:destroyed, key, :normal} -> assert key == entity_key
      after 100 -> assert false
    end
  end

  test "has component" do
    entity_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4, owner: self())

    assert Lkn.Core.Entity.has_component?(entity_key, Test.System)
    assert !Lkn.Core.Entity.has_component?(entity_key, Test.NoSystem)
  end

  test "spawning entity and read level" do
    entity_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4, owner: self())
    Option.some(4) = Lkn.Core.Entity.read(entity_key, :level)
  end

  test "spawning puppeteer just to stop it" do
    puppeteer_key = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key)

    Lkn.Core.Puppeteer.stop(puppeteer_key)

    receive do
      {:destroyed, key} -> assert puppeteer_key == key
      after 100 -> raise "should have received a destroy notification already"
    end
  end

  test "spawning puppeteer and testing its private cast" do
    puppeteer_key = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key)

    Test.Puppeteer.wizz(puppeteer_key, 2)

    receive do
      {:wizz, n, x} -> assert x == puppeteer_key && n == 0
      after 100 -> raise "Waiting for 100ms the message {:wizz, 0, #{inspect puppeteer_key}}"
    end

    receive do
      {:wizz, n, x} -> assert x == puppeteer_key && n == 1
      after 100 -> raise "Waiting for 100ms the message {:wizz, 1, #{inspect puppeteer_key}}"
    end
  end

  test "spawning everything" do
    map_key = UUID.uuid4()
    puppeteer_key = UUID.uuid4()
    puppeteer_key2 = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key)
    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key2)

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Puppeteer.find_instance(puppeteer_key, map_key)
    instance_key2 = Puppeteer.find_instance(puppeteer_key2, map_key)

    assert instance_key == instance_key2

    Lkn.Core.Puppeteer.leave_instance(puppeteer_key2, instance_key)

    receive do
      :kick -> :ok
      after 100 -> assert false
    end

    Process.sleep 10

    entity_key = UUID.uuid4()
    ghost_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4, owner: self())
    {:ok, _} = Test.Ghost.start_link(ghost_key)

    0 = Lkn.Core.System.population_size(instance_key, Test.System)
    Lkn.Core.Instance.register_puppet(instance_key, entity_key)
    1 = Lkn.Core.System.population_size(instance_key, Test.System)

    receive do
      {:enter, x} -> assert x == entity_key
      after 100 -> raise "Waiting for 100ms the message {:enter, #{inspect entity_key}}"
    end

    Lkn.Core.Instance.register_puppet(instance_key, ghost_key)
    1 = Lkn.Core.System.population_size(instance_key, Test.System)

    receive do
      x -> raise "we have received #{inspect x}, when we shouldn’t"
      after 100 -> :ok
    end

    Lkn.Core.Instance.unregister_puppet(instance_key, entity_key)
    Process.sleep 10
    0 = Lkn.Core.System.population_size(instance_key, Test.System)

    receive do
      {:leave, x} -> assert x == entity_key
      after 100 -> assert false
    end
  end

  test "level up" do
    map_key = UUID.uuid4()
    puppeteer_key = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key)

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Puppeteer.find_instance(puppeteer_key, map_key)

    entity_key = UUID.uuid4()

    {:ok, _} = Test.Entity.start_link(entity_key, name: "lkn", level: 4, owner: self())

    Lkn.Core.Instance.register_puppet(instance_key, entity_key)

    Test.System.level_up(instance_key, entity_key)

    receive do
      {:enter, x} -> assert x == entity_key
      after 100 -> raise "Waiting for 100ms the message {:enter, #{inspect entity_key}}"
    end

    receive do
      {:level_up, {4, 5}} -> :ok
      after 100 -> raise "Waiting for 100ms the message {:level_up, {4, 5}}"
    end
  end

  test "register then unregister" do
    map_key = UUID.uuid4()
    puppeteer_key = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key)

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Puppeteer.find_instance(puppeteer_key, map_key)

    Puppeteer.leave_instance(puppeteer_key, instance_key)
    Process.sleep(10)
    instance_key2 = Puppeteer.find_instance(puppeteer_key, map_key)

    assert(instance_key2 == instance_key)

    Puppeteer.leave_instance(puppeteer_key, instance_key)
    Process.sleep(110)
    instance_key2 = Puppeteer.find_instance(puppeteer_key, map_key)

    assert(instance_key2 != instance_key)
  end

  test "three registers" do
    map_key = UUID.uuid4()
    puppeteer_key1 = UUID.uuid4()
    puppeteer_key2 = UUID.uuid4()
    puppeteer_key3 = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key1)
    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key2)
    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key3)

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key1 = Puppeteer.find_instance(puppeteer_key1, map_key)
    instance_key2 = Puppeteer.find_instance(puppeteer_key2, map_key)

    assert(instance_key2 == instance_key1)

    instance_key3 = Puppeteer.find_instance(puppeteer_key3, map_key)

    assert(instance_key3 != instance_key1)
  end

  test "lock then try to register" do
    map_key = UUID.uuid4()
    puppeteer_key1 = UUID.uuid4()
    puppeteer_key2 = UUID.uuid4()

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key1)
    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key2)

    instance_key1 = Puppeteer.find_instance(puppeteer_key1, map_key)

    Instance.lock(instance_key1)

    instance_key2 = Puppeteer.find_instance(puppeteer_key2, map_key)

    assert(instance_key1 != instance_key2)
  end

  test "register then unregister (no delay for the map)" do
    map_key = UUID.uuid4()
    puppeteer_key = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key)

    {:ok, _} = Test.Map.start_link(map_key, Option.nothing())
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Puppeteer.find_instance(puppeteer_key, map_key)
    Puppeteer.leave_instance(puppeteer_key, instance_key)

    Process.sleep(10)

    instance_key2 = Puppeteer.find_instance(puppeteer_key, map_key)

    assert(instance_key2 != instance_key)
  end

  test "register, lock then unregister" do
    map_key = UUID.uuid4()
    puppeteer_key = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key)

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Puppeteer.find_instance(puppeteer_key, map_key)

    [{pid, _}] = Registry.lookup(Lkn.Core.Registry, {:instance, instance_key})
    Process.monitor(pid)

    Instance.lock(instance_key)

    Puppeteer.leave_instance(puppeteer_key, instance_key)

    receive do
      {:DOWN, _, _, _, _} -> :ok
      after 100 -> assert false
    end
  end

  test "register, lock then kick" do
    map_key = UUID.uuid4()
    puppeteer_key = UUID.uuid4()

    {:ok, _} = Test.Puppeteer.start_link(puppeteer_key)

    {:ok, _} = Test.Map.start_link(map_key)
    :ok = Lkn.Core.Pool.spawn_pool(map_key)

    instance_key = Puppeteer.find_instance(puppeteer_key, map_key)

    [{pid, _}] = Registry.lookup(Lkn.Core.Registry, {:instance, instance_key})
    Process.monitor(pid)

    Instance.lock(instance_key)
    Instance.kick_all(instance_key)

    receive do
      :kick -> :ok
      after 100 -> assert false
    end

    receive do
      {:DOWN, _, _, _, _} -> :ok
      after 100 -> assert false
    end
  end
end

######################################################################
#                              SYSTEM                                #
#                                                                    #
import Lkn.Core.Component, only: [defcomponent: 2]
import Lkn.Core.Map, only: [defmap: 2]
import Lkn.Core.Puppet, only: [defpuppet: 2]
import Lkn.Core.Puppeteer, only: [defpuppeteer: 2]
import Lkn.Core.System, only: [defsystem: 2]

defcomponent Test.System.Puppet do
  @system Test.System

  @doc """
  Test
  """
  @call level_up() :: {number, number}
end

defcomponent Test.System.Map do
  @system Test.System

  @doc """
  Test
  """
  @call level_max() :: number
end

defsystem Test.System do
  @puppet Test.System.Puppet
  @map Test.System.Map

  def init_state(_instance_key, _map_key) do
    :ok
  end

  def start_link(instance_key, map_key) do
    Lkn.Core.System.start_link(__MODULE__, instance_key, map_key)
  end

  def puppet_enter(:ok, _instance_key, _map_key, _entities, key) do
    notify(&(Test.Puppeteer.Specs.emit(&1, {:enter, key})))
    :ok
  end

  def puppet_leave(:ok, _instance_key, _map_key, _entities, key) do
    notify(&(Test.Puppeteer.Specs.emit(&1, {:leave, key})))
    :ok
  end

  cast level_up(puppet_key :: Puppet.k) do
    notif = Test.System.Puppet.level_up(puppet_key)
    notify(&(Test.Puppeteer.Specs.emit(&1, {:level_up, notif})))

    state
  end
end

######################################################################
#                                MAP                                 #
#                                                                    #
defmodule Test.Map.Component do
  use Test.System.Map

  def level_max(_, _), do: 7

  def init_state(_), do: {:ok, :ok}
end

defmap Test.Map do
  @components [Test.Map.Component]

  def start_link(key, delay \\ Option.some(100)) do
    Lkn.Core.Entity.start_link(__MODULE__, key, delay)
  end

  def init_properties(delay) do
    case delay do
      Option.some(delay) ->
        %{:delay => delay, :limit => 2}
      Option.nothing() ->
        %{:limit => 2}
    end
  end

  def digest(_props) do
    %{}
  end

  def destroy(_key, _props, _reason) do
    :ok
  end
end

######################################################################
#                              ENTITY                                #
#                                                                    #
defmodule Test.Entity.Component do
  use Test.System.Puppet

  def init_state(_), do: {:ok, :ok}

  def level_up(entity_key, :ok) do
    Option.some(lvl) = read(entity_key, :level)

    write(entity_key, :level, lvl + 1)

    {{lvl, lvl + 1}, :ok}
  end
end

defpuppet Test.Entity do
  @components [Test.Entity.Component]

  def start_link(key, args = [name: _name, level: _level, owner: _owner]) do
    Lkn.Core.Entity.start_link(__MODULE__, key, args)
  end

  def init_properties(name: name, level: level, owner: owner) do
    %{:name => name, :level => level, :owner => owner}
  end

  def digest(props) do
    props
  end

  def destroy(key, props, reason) do
    target = Map.get(props, :owner)
    send target, {:destroyed, key, reason}
    :ok
  end
end

defpuppet Test.Ghost do
  @components []

  def start_link(key) do
    Lkn.Core.Entity.start_link(__MODULE__, key, [])
  end

  def init_properties([]) do
    %{}
  end

  def digest(props) do
    props
  end

  def destroy(_key, _props, _reason) do
    :ok
  end
end

######################################################################
#                             PUPPETEER                              #
#                                                                    #

defpuppeteer Test.Puppeteer.Specs do
  @cast emit(msg :: any)
end

defmodule Test.Puppeteer do
  use Test.Puppeteer.Specs do
    cast wizz(n :: number) do
      for i <- 0..(n-1) do
        send state, {:wizz, i, key}
      end
      state
    end
  end

  def start_link(puppeteer_key) do
    Puppeteer.start_link(__MODULE__, puppeteer_key, self())
  end

  def init_state(s) do
    {:ok, s}
  end

  def instance_digest(state, _instance_key, map_key, _map, _puppets) do
    state
  end

  def puppet_enter(state, _instance_key, _puppet_key, _digest) do
    state
  end

  def puppet_leave(state, _instance_key, _puppet_key) do
    state
  end

  def destroy(puppeteer_key, state, _instance_key, _reason) do
    send state, {:destroyed, puppeteer_key}
  end

  def leave_instance(target, _instance_key) do
    send(target, :kick)
    target
  end

  def emit(_puppeteer_key, msg, instance_key, target) do
    send target, msg
    target
  end
end


######################################################################
#                             TEST COMP                              #
#                                                                    #
defcomponent Test.C do
  @moduledoc """
  Dummy component to test the defspecs macro in depth.
  """

  @system NoBody

  # cast without any doc nor args
  @cast f1()

  # cast without any doc
  @cast f2(x :: number, y :: String.t)

  # cast with doc but without args
  @doc """
  Hi.
  """
  @cast f3()

  # cast with doc and args
  @doc """
  Hi.
  """
  @cast f4(x :: number, y :: String.t)

  # call without any doc nor args
  @call f5() :: boolean

  # call without any doc
  @call f6(z :: number, a :: String.t) :: no_return

  # call with doc, no args
  @doc """
  Hi.
  """
  @call f7() :: boolean

  # call with doc and args
  @doc """
  Hi.
  """
  @call f8(z :: number, a :: String.t) :: no_return
end
