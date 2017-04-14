defmodule Lkn.Srv do
  import Supervisor.Spec

  def start(_type, _args) do
    children = [
      supervisor(Task.Supervisor, [[name: Lkn.Srv.Tasks]]),
      worker(Task, [Lkn.Srv.Play, :server, [4000]]),
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Lkn.Srv)
  end

  def task(lambda) do
    Task.Supervisor.start_child(Lkn.Srv.Tasks, lambda)
  end
end
