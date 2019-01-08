
```
sets pancake: image

alias temperature = context(urn:lemonpi:context:weather:temperature)

pancake = "http://www.x.com/default.png"

if temperature() > 10:
  pancake = "http://www.x.com/warm-pancake.png"

```


```
let
  temperature = external(urn:lemonpi:context:weather:temperature)
  city = external(urn:lemonpi:context:location:city)
in
  if temperature() > 10:
    title = "Welcome to ${city()}"
``


