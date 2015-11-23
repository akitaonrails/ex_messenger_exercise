defmodule ExMessenger do
  use Application

  def start(_type, _args) do
    ExMessenger.Supervisor.start_link
  end
end
