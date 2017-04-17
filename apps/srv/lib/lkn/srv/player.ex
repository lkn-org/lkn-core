defmodule Lkn.Srv.Player do
  alias Lkn.Core.Entity
  alias Lkn.Core.Instance
  alias Lkn.Core.Name
  alias Lkn.Core.Pool
  alias Lkn.Core.Puppeteer

  alias Lkn.Rpg.Action
  alias Lkn.Rpg.Puppet

  alias Lkn.Srv.Command, as: C
  alias Lkn.Srv.Notification, as: N

  alias Socket.Web

  require Logger

  defmodule State do
    defstruct [
      :puppeteer_key,
      :instance_key,
      :puppet_key,
      :map_key,
      :socket,
    ]

    @type t :: %State{
      puppeteer_key: Puppeteer.t,
      instance_key: Instance.t,
      puppet_key: Entity.t,
      map_key: Entity.t,
      socket: any,
    }

    def new(socket, puppeteer_key, map_key) do
      puppet_key = UUID.uuid4()
      Puppet.start_link(puppet_key, mute: false)

      instance_key = Pool.register_puppeteer(map_key, puppeteer_key, __MODULE__)
      Instance.register_entity(instance_key, puppet_key)

      %State{
        socket: socket,
        puppeteer_key: puppeteer_key,
        instance_key: instance_key,
        puppet_key: puppet_key,
        map_key: map_key
      }
    end
  end

  use GenServer

  def start_link(socket, puppeteer_key, map_key) do
    GenServer.start_link(__MODULE__, {socket, puppeteer_key, map_key}, name: Name.puppeteer(puppeteer_key))
  end

  def init({socket, puppeteer_key, map_key}) do
    {:ok, State.new(socket, puppeteer_key, map_key)}
  end

  def process(puppeteer_key, msg) do
    GenServer.cast(Name.puppeteer(puppeteer_key), {:cmd, msg})
  end

  def kill(puppeteer_key) do
    GenServer.cast(Name.puppeteer(puppeteer_key), :kill)
  end

  def handle_cast(:kill, state) do
    Instance.unregister_entity(state.instance_key, state.puppet_key)
    Instance.unregister_puppeteer(state.instance_key, state.puppeteer_key)
    {:stop, :normal, state}
  end
  def handle_cast({:cmd, %C.Talk{msg: msg}}, state) do
    Action.talk(state.instance_key, state.puppet_key, msg)
    {:noreply, state}
  end

  @behaviour Puppeteer

  def force_unregister(puppeteer_key, from: instance_key) do
    Logger.info("[Puppeteer(#{inspect puppeteer_key})] needs to unregister itself ([Instance(#{inspect instance_key})]")
  end

  def handle_info({:notify, {:talk, e, msg}}, state) do
    Web.send!(
      state.socket,
      {:text, N.serialize!(N.Talk.new(e, msg))}
    )
    {:noreply, state}
  end
end
