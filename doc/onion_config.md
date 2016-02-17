

# Module onion_config #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Application Configuration API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_boolean-2">get_boolean/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_boolean-3">get_boolean/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_integer-2">get_integer/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_integer-3">get_integer/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_string-2">get_string/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_string-3">get_string/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_boolean-2"></a>

### get_boolean/2 ###

<pre><code>
get_boolean(Application, Key) -&gt; Result | not_found
</code></pre>

<ul class="definitions"><li><code>Application = atom()</code></li><li><code>Key = atom()</code></li><li><code>Result = boolean()</code></li></ul>

<a name="get_boolean-3"></a>

### get_boolean/3 ###

<pre><code>
get_boolean(Application, Key, Default) -&gt; Result | not_found
</code></pre>

<ul class="definitions"><li><code>Application = atom()</code></li><li><code>Key = atom()</code></li><li><code>Default = term()</code></li><li><code>Result = boolean()</code></li></ul>

<a name="get_integer-2"></a>

### get_integer/2 ###

<pre><code>
get_integer(Application, Key) -&gt; Result | not_found
</code></pre>

<ul class="definitions"><li><code>Application = atom()</code></li><li><code>Key = atom()</code></li><li><code>Result = integer()</code></li></ul>

<a name="get_integer-3"></a>

### get_integer/3 ###

<pre><code>
get_integer(Application, Key, Default) -&gt; Result | not_found
</code></pre>

<ul class="definitions"><li><code>Application = atom()</code></li><li><code>Key = atom()</code></li><li><code>Default = term()</code></li><li><code>Result = integer()</code></li></ul>

<a name="get_string-2"></a>

### get_string/2 ###

<pre><code>
get_string(Application, Key) -&gt; Result | not_found
</code></pre>

<ul class="definitions"><li><code>Application = atom()</code></li><li><code>Key = atom()</code></li><li><code>Result = string()</code></li></ul>

<a name="get_string-3"></a>

### get_string/3 ###

<pre><code>
get_string(Application, Key, Default) -&gt; Result | not_found
</code></pre>

<ul class="definitions"><li><code>Application = atom()</code></li><li><code>Key = atom()</code></li><li><code>Default = term()</code></li><li><code>Result = string()</code></li></ul>

<a name="get_value-2"></a>

### get_value/2 ###

<pre><code>
get_value(Application, Key) -&gt; Result | not_found
</code></pre>

<ul class="definitions"><li><code>Application = atom()</code></li><li><code>Key = atom()</code></li><li><code>Result = term()</code></li></ul>

<a name="get_value-3"></a>

### get_value/3 ###

<pre><code>
get_value(Application, Key, Default) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Application = atom()</code></li><li><code>Key = atom()</code></li><li><code>Default = term()</code></li><li><code>Result = term()</code></li></ul>

