import time
from huey import RedisHuey

huey = RedisHuey("predictions", host="localhost", port=16379)


@huey.task()
def predict(features):
    time.sleep(5)
    s = sum(n for n in features.values())
    print("Predicted:", s)
    return s


def create_prediction(features):
    return predict(features)
