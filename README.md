# Code Inspector

Programming language detector algorithm.

Key feature: the algorithm is aimed on classifying *short* and possibly *incomplete* code snippets,
without any prior knowledge of file extension, context, keywords, etc.

Currently recognized languages: 
c, c++, c#, clojure, coffeescript, css, erlang, f#, go, html, java, javascript, lua, matlab, objective c, 
pascal, perl, php, python, r, ruby, scala, shell, sql, swift, typescript, visual basic.

Snippets examples
-----------------

#### Java

```java
BigInteger value = bd.unscaledValue();
char[] buffer = value.abs().toString().toCharArray();
int numDigitsLeft = buffer.length - 1;
int endPosition = numDigitsLeft % 2;
int length = (precision + 2) / 2;
```

#### JavaScript

```javascript
var _a = this, source = _a.source, count = _a.count;
if (count === 0) {
    return _super.prototype.error.call(this, err);
}
```

#### C++

```cpp
unsigned int mw = 0;
int support = -1;
if (hnvml) {
	support = nvml_get_power_usage(hnvml, gpu->gpu_id, &mw);
}
```

#### C

```c
    BOOL ret;
    DWORD err;

    if (!PyArg_ParseTuple(args, F_HANDLE "O", &handle, &bufobj))
      return NULL;
```

#### Python

```python
    self.path = base_path
    self.target = spec['target_name']
    self.type = spec['type']
    self.toolset = spec['toolset']
```

#### PHP

```php
    {
        $adapter = new Ftp($this->options + ['systemType' => 'unknown']);
        $adapter->listContents();
    }
```

Clearly, not all languages can be distinguished within a short snippet, see for example **C** and **C++**, that's why
the model outputs a probability distribution. The larger the snippet, the more confident the model is.

License
-------

[Apache 2.0](LICENSE)
