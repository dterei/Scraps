# RabbitMQ - AMQP Play

Make sure rabbitmq is installed and running locally.

## Cheat Sheet

List queues:

```sh
sudo rabbitmqctl list_queues
```

List queues with more details:

```sh
sudo rabbitmqctl list_queues name messages_ready messages_unacknowledged
```

List bindings:

```sh
sudo rabbitmqctl list_bindings
```

List exchanges:

```sh
sudo rabbitmqctl list_exchanges
```

Restart server:

```sh
sudo rabbitmqctl stop_app
sudo rabbitmqctl start_app
```

Restart server clearing all queues:

```sh
sudo rabbitmqctl stop_app
sudo rabbitmqctl restart
sudo rabbitmqctl start_app
```
