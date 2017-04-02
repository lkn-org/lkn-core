defmodule Lkn.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path:       "apps",
      build_embedded:  Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps:            deps(),
      test_coverage:   [tool: ExCoveralls],
      preferred_cli_env: [
        "coveralls":        :test,
        "coveralls.detail": :test,
        "coveralls.post":   :test,
        "coveralls.html":   :test,
      ],
      dialyzer:        [
        flags: [
          "-Wunmatched_returns",
          :error_handling,
          :race_conditions,
        ],
      ],
    ]
  end

  defp deps do
    [
      {:credo,       "~> 0.4",  only: [:dev, :test], runtime: false},
      {:dialyxir,    "~> 0.5",  only: :dev,          runtime: false},
      {:ex_doc,      "~> 0.15", only: :dev,          runtime: false},
      {:excoveralls, "~> 0.6",  only: :test,         runtime: false}
    ]
  end
end
