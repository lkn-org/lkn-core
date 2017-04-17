defmodule Srv.Mixfile do
  use Mix.Project

  def project do
    [
      app:         :srv,
      version:     "0.1.0",
      build_path:  "../../_build",
      config_path: "../../config/config.exs",
      deps_path:   "../../deps",
      lockfile:    "../../mix.lock",
      elixir:      "~> 1.4",
      build_embedded:
                   Mix.env == :prod,
      start_permanent:
                   Mix.env == :prod,
      deps:        deps(),
      test_coverage:
                   [
                     tool: ExCoveralls,
                   ],
    ]
  end

  def application do
    [
      extra_applications: [
        :logger,
      ],
      mod: {Lkn.Srv, []},
    ]
  end

  defp deps do
    [
      {:rpg, in_umbrella: true},
      {:foundation, in_umbrella: true},
      {:core, in_umbrella: true},
      {:uuid, "~> 1.1"},
      {:socket, "~> 0.3"},
      {:poison, "~> 3.0"},
    ]
  end
end
