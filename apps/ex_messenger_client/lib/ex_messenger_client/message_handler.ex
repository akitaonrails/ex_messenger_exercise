defmodule ExMessengerClient.MessageHandler do
  use GenServer

  def start_link(server) do
    :gen_server.start_link({ :local, :message_handler }, __MODULE__, server, [])
  end

  def init(server) do
    { :ok, server }
  end

  def handle_cast({ :message, nick, message }, server) do
    message = message |> String.rstrip
    IO.puts "\n#{server}> #{nick}: #{message}"
    IO.write "#{Node.self}> "
    {:noreply, server}
  end
end