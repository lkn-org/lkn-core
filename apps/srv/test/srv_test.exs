defmodule SrvTest do
  use ExUnit.Case

  setup_all do
    Application.stop(:srv)
    Application.start(:srv)
    :ok
  end

  test "connect, speak" do
    client = Socket.connect!("ws://localhost:4000")
    Socket.Web.send!(client, {:text, "hi"})

    {:text, _msg} = Socket.Web.recv!(client)
  end

  test "connect, speak, connect then speak" do
    client1 = Socket.connect!("ws://localhost:4000")
    Socket.Web.send!(client1, {:text, "hi"})

    {:text, _msg} = Socket.Web.recv!(client1)

    client2 = Socket.connect!("ws://localhost:4000")
    Socket.Web.send!(client2, {:text, "hi"})

    {:text, msg1} = Socket.Web.recv!(client1)
    {:text, msg2} = Socket.Web.recv!(client2)

    assert (msg1 == msg2)
  end
end
