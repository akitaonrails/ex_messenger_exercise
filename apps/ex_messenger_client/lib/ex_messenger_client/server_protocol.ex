defmodule ExMessengerClient.ServerProcotol do
  def connect({server, nick}) do
    server |> call({:connect, nick})
  end

  def disconnect({server, nick}) do
    server |> call({:disconnect, nick})
  end

  def list_users({server, nick}) do
    server |> cast({:list_users, nick})
  end

  def private_message({server, nick}, to, message) do
    server |> cast({:private_message, nick, to, message})
  end

  def say({server, nick}, message) do
    server |> cast({:say, nick, message})
  end

  defp call(server, args) do
    GenServer.call({:message_server, server}, args)
  end

  defp cast(server, args) do
    GenServer.cast({:message_server, server}, args)
  end
end