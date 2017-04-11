defmodule RpgTest do
  use ExUnit.Case, async: false

  doctest Lkn.Rpg.Map
  doctest Lkn.Rpg.Puppet

  alias Lkn.Core.Instance
  alias Lkn.Core.Name
  alias Lkn.Core.Pool
  alias Lkn.Core.Puppeteer

  alias Lkn.Rpg.Action
  alias Lkn.Rpg.Map
  alias Lkn.Rpg.Puppet

  defmodule Test.Puppeteer do
    @behaviour Puppeteer

    use GenServer

    def start_link(puppeteer_key) do
      GenServer.start_link(__MODULE__, {puppeteer_key, self()}, name: Name.puppeteer(puppeteer_key))
    end

    def init(s) do
      {:ok, s}
    end

    def force_unregister(_puppeteer_key, from: _instance_key) do
    end

    def find_instance(puppeteer_key, map_key) do
      GenServer.call(Name.puppeteer(puppeteer_key), {:find_instance, map_key})
    end

    def leave_instance(puppeteer_key, instance_key) do
      GenServer.cast(Name.puppeteer(puppeteer_key), {:leave_instance, instance_key})
    end

    def handle_info(msg, state = {_pk, target}) do
      send(target, msg)

      {:noreply, state}
    end

    def handle_call({:find_instance, map_key}, _from, state = {pk, _target}) do
      {:reply, Lkn.Core.Pool.register_puppeteer(map_key, pk, __MODULE__), state}
    end

    def handle_cast({:leave_instance, instance_key}, state = {pk, _target}) do
      Lkn.Core.Instance.unregister_puppeteer(instance_key, pk)

      {:noreply, state}
    end
  end

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
    puppeteer_key = UUID.uuid4()

    Map.start_link(map_key, delay: 100, limit: 10, mute: instance_mute)
    Test.Puppeteer.start_link(puppeteer_key)
    Puppet.start_link(puppet_key, mute: puppet_mute)

    Pool.spawn_pool(map_key)

    instance_key = Test.Puppeteer.find_instance(puppeteer_key, map_key)
    Instance.register_entity(instance_key, puppet_key)

    Action.talk(instance_key, puppet_key, "hi")
  end
end
