defmodule Chatterbox.Application do
  use Application

  def start(_type, _args) do
    children = [
      %{id: Chatterbox.Supervisor, start: {:chatterbox_supervisor@ps, :start_link, []}}
    ]

    opts = [strategy: :one_for_one, name: Chatterbox.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
