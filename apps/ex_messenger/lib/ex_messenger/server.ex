defmodule ExMessenger.Server do
  use GenServer
  require Logger

  def start_link([]) do
    :gen_server.start_link({ :local, :message_server }, __MODULE__, [], [])
  end

  def init([]) do
    { :ok, HashDict.new }
  end

  def say(nick, message) do
    GenServer.cast(:message_server, { :say, nick, "* #{nick} #{message} *" })
  end

  def handle_call({ :connect, nick }, {from, _} , users) do
    cond do
      nick == :server or nick == "server" ->
        {:reply, :nick_not_allowed, users}
      HashDict.has_key?(users, nick) ->
        {:reply, :nick_in_use, users}
      true ->
        new_users = users |> HashDict.put(nick, node(from))
        user_list = log(new_users, nick, "has joined")
        {:reply, { :ok, user_list }, new_users}
    end
  end

  def handle_call({ :disconnect, nick }, {from, _}, users) do
    user = HashDict.get(users, nick)
    cond do
      user == nil ->
        {:reply, :user_not_found, users}
      user == node(from) ->
        new_users = users |> HashDict.delete(nick)
        log(new_users, nick, "has left")
        {:reply, :ok, new_users }
      true ->
        {:reply, :not_allowed, users }
    end
  end

  defp log(users, nick, message) do
    user_list = users |> HashDict.keys |> Enum.join(":")
    Logger.debug("#{nick} #{message}, user_list: #{user_list}")
    say(nick, message)
    user_list
  end

  def handle_cast({ :say, nick, message }, users) do
    ears = HashDict.delete(users, nick)
    Logger.debug("#{nick} said #{message}")
    broadcast(ears, nick, message)
    {:noreply, users}
  end

  def handle_cast({ :private_message, nick, receiver_nick, message }, users) do
    case HashDict.get(users, receiver_nick) do
      nil -> :ok
      receiver_node ->
        send_message_to_client(receiver_node, nick, message)
    end
    {:noreply, users}
  end

  def handle_cast({ :list_users, nick }, users) do
    user_list = users |> HashDict.keys |> Enum.join(", ")
    GenServer.cast( :message_server, { :private_message, "server", nick, "users: #{user_list}"})
    {:noreply, users}
  end

  defp broadcast(users, nick, message) do
    Enum.map(users, fn {_, node} ->
      Task.async(fn ->
        send_message_to_client(node, nick, message)
      end)
    end)
    |> Enum.map(&Task.await/1)
  end

  defp send_message_to_client(client_node, nick, message) do
    GenServer.cast({ :message_handler, client_node }, { :message, nick, message })
  end
end