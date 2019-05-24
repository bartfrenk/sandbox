import asyncio
from aiohttp import web
from aioredis import create_connection

loop = asyncio.get_event_loop()


async def listen():
    conn = await create_connection("redis://localhost:16379/0")
    event = await conn.execute("blpop", "channel", "0")
    print(event)
    conn.close()
    await conn.wait_closed()


async def index(_request):
    return web.Response(text="Hello World!")


def setup_routes(app):
    app.router.add_get("/", index)


async def main(app):
    asyncio.gather(listen(), app.startup())


# TODO: Register listener to Redis queue in the event loop
# TODO: Add celery integration
# TODO: Generate swagger endpoints (maybe add Resource abstraction)
# TODO: Look for async framework that includes all of the above
# TODO: Check performance - understand how well this matches our use case

app = web.Application()
setup_routes(app)
asyncio.run(main(app))
