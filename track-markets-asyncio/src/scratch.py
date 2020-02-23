import logging
import asyncio

import websockets


log = logging.getLogger(__name__)


class MarketStream(websockets.client.Connect):

    BASE_URI = "wss://stream.binance.com:9443/stream?streams=ethbtc@depth"

    def __init__(self, market, **kwargs):
        self._market = market
        super().__init__(self.uri, **kwargs)

    @property
    def uri(self):
        path = f"/stream?streams={0}@depth/{0}@trade".format(self._market)
        return self.BASE_URI + path


class TradesWriter:
    def __init__(self, store):
        self._store = store
        self._queue = None

    async def start(self):
        self._queue = asyncio.Queue()

    # pylint: disable=method-hidden
    async def _writer(self):
        while True:
            trade = await self._queue.get()

    async def write(self, trade):
        self._queue.put(trade)


async def listen(market, store):
    try:
        stream = await MarketStream(market).start()
        trades_writer = await TradesWriter(store).start()
        while True:
            message = await stream.recv()
            data = message.get("data")
            if data.get("e") == "trade":
                await trades_writer.write(data)
            else:
                log
    finally:
        stream.close()
        trades_writer.close()
        order_book_writer.close()


asyncio.get_event_loop().run_until_complete(listen())
