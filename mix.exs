defmodule Lkn.Mixfile do
  use Mix.Project

  def project do
    [
      app:             :lkn_core,
      name:            "lkn-core",
      version:         "0.4.3",
      elixir:          "~> 1.5",
      build_embedded:  Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      source_url:      "https://nest.pijul.com/lthms/lkn-core",
      description:     description(),
      deps:            deps(),
      package:         package(),
      test_coverage:   [
        tool: ExCoveralls
      ],
      preferred_cli_env: [
        "coveralls":        :test,
        "coveralls.detail": :test,
        "coveralls.post":   :test,
        "coveralls.html":   :test,
      ],
      docs: [
        extras: [
          "ChangeLog.md",
        ],
      ],
      dialyzer:        [
        flags: [
          :error_handling,
          :race_conditions,
        ],
      ],
    ]
  end

  def application do
    [
      extra_applications: [
        :logger,
      ],
      mod:                {Lkn.Core, []},
    ]
  end

  defp deps do
    [
      # runtime
      {:uuid, "~> 1.1"},
      {:lkn_prelude, "~> 0.1.2"},
      {:beacon,      "~> 1.1"},

      # development
      {:credo,       "~> 0.9",  only: [:dev, :test], runtime: false},
      {:dialyxir,    "~> 0.5",  only: :dev,          runtime: false},
      {:ex_doc,      "~> 0.18", only: :dev,          runtime: false},
      {:excoveralls, "~> 0.8",  only: :test,         runtime: false},
    ]
  end

  defp description do
    """
    Yet another ECS, actor-based and focused on getting ride of as much
    boilerplate as possible.
    """
  end

  defp package do
    [
      name: :lkn_core,
      files: [
        "lib",
        "mix.exs",
        "README.md",
        "ChangeLog.md",
        "LICENSE",
      ],
      maintainers: [
        "Thomas Letan"
      ],
      licenses: [
        "AGPL 3.0"
      ],
      links: %{
        "Pijul Nest" => "https://nest.pijul.com/lthms/lkn-core",
      },
    ]
  end
end
