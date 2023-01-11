defmodule Chatterbox.MixProject do
  use Mix.Project

  def project do
    [
      app: :chatterbox,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:purerl | Mix.compilers()],
      erlc_paths: ["output"]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Chatterbox.Application, []},
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      {:purerlex, "~> 0.4.2"},
      {:jsx, "~> 3.1"},
      {:cowboy, "~> 2.9.0"}
    ]
  end
end
