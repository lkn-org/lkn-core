defmodule RpgTest do
  use ExUnit.Case, async: false

  doctest Lkn.Rpg.Map
  doctest Lkn.Rpg.Puppet

  alias Lkn.Core.Instance
  alias Lkn.Core.Name
  alias Lkn.Core.Pool

  alias Lkn.Rpg.Action
  alias Lkn.Rpg.Map
  alias Lkn.Rpg.Puppet

  test "spawn a map" do
    map_key = UUID.uuid4()

    Map.start_link(map_key, delay: 100, limit: 10, mute: false)
  end

  test "spawn a puppet" do
    puppet_key = UUID.uuid4()

    Puppet.start_link(puppet_key, mute: false)
  end

  test "spawn an instance and register our puppet" do
    puppet_key = UUID.uuid4()
    talk_scenario(puppet_key, false, false)

    receive do
      {:notify, {:talk, p,  x}} -> assert (p == puppet_key && x == "hi")
      after 100 -> assert false
    end
  end

  test "muted instance are silent" do
    puppet_key = UUID.uuid4()
    talk_scenario(puppet_key, true, false)

    receive do
      {:notify, {:talk, _p,  _x}} -> assert false
      after 100 -> :ok
    end
  end

  test "muted puppet are silent" do
    puppet_key = UUID.uuid4()
    talk_scenario(puppet_key, false, true)

    receive do
      {:notify, {:talk, _p,  _x}} -> assert false
      after 100 -> :ok
    end
  end

  def talk_scenario(puppet_key, instance_mute, puppet_mute) do
    map_key = UUID.uuid4()

    Map.start_link(map_key, delay: 100, limit: 10, mute: instance_mute)
    Puppet.start_link(puppet_key, mute: puppet_mute)

    Pool.spawn_pool(map_key)

    instance_key = Lkn.Core.Pool.register_puppeteer(map_key, :actor)
    Registry.register(Lkn.Core.Notifier, Name.notify_group(instance_key), [])

    Instance.register_entity(instance_key, puppet_key)

    Action.talk(instance_key, puppet_key, "hi")
  end
end
