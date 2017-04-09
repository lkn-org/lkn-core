defmodule Rpg.Mixfile do
  use Mix.Project

  def project do
    [app: :rpg,
     version: "0.1.0",
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     test_coverage: [
       tool: ExCoveralls,
     ],
     deps: deps()]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:foundation, in_umbrella: true},
      {:core, in_umbrella: true},
      {:uuid, "~> 1.1"},
    ]
  end
end
