import json

from redis import Redis

import app.interactions as i

client = Redis(host="localhost", port=16379, db=1)


def listen():
    while True:
        print("Listening for events")
        packed = client.blpop(["test"], 30)
        if not packed:
            continue
        data = json.loads(packed[1].decode("utf-8"))
        # Sign-off functionality for redis is a noop, hence a message
        # handle is not needed: hence None
        yield data


for data in listen():
    prediction = i.create_prediction(data)
    print("Created prediction: {}".format(prediction))
