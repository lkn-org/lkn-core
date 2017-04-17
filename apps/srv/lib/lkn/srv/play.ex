defmodule Lkn.Srv.Play do
  alias Socket.Web

  alias Lkn.Core.Pool

  alias Lkn.Rpg.Map

  alias Lkn.Srv
  alias Lkn.Srv.Player
  alias Lkn.Srv.Command

  def server(port) do
    # setup the bare minimum for one map
    map_key = UUID.uuid4()
    Map.start_link(map_key, delay: 5000, limit: 10, mute: false)
    Pool.spawn_pool(map_key)

    # listen for incoming connection
    server = Web.listen!(port)
    loop_acceptor(server, map_key)
  end

  defp loop_acceptor(server, map_key) do
    client = Web.accept!(server)

    Srv.task(fn -> serve(client, map_key) end)
    loop_acceptor(server, map_key)
  end

  defp serve(client, map_key) do
    Socket.Web.accept!(client)

    puppeteer_key = UUID.uuid4()
    Player.start_link(client, puppeteer_key, map_key)

    recv(puppeteer_key, client)
  end

  defp recv(puppeteer_key, client) do
    case Web.recv(client) do
      {:ok, {:text, msg}} ->
        try do
          cmd = Command.parse!(msg)
          Player.process(puppeteer_key, cmd)
        rescue
          e in _ ->
            Web.send!(client, {:text, "WRONG COMMAND #{inspect msg}: #{inspect e}"})
        end

        recv(puppeteer_key, client)
      _ ->
        Player.kill(puppeteer_key)
    end
  end
end
