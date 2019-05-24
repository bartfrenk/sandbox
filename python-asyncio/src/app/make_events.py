import json

from redis import Redis

client = Redis(host="localhost", port=16379, db=1)

data = {"a": 10, "b": 20}

client.rpush("test", json.dumps(data))
