

# Module onion_math #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Math API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ceil-1">ceil/1</a></td><td>Round a given number upwards towards to the nearest integer.</td></tr><tr><td valign="top"><a href="#floor-1">floor/1</a></td><td>Round a given number downwards towards the nearest integer.</td></tr><tr><td valign="top"><a href="#mod-2">mod/2</a></td><td>Return X mod Y.</td></tr><tr><td valign="top"><a href="#mod_pow-3">mod_pow/3</a></td><td>Return B^E mod M.</td></tr><tr><td valign="top"><a href="#pow-2">pow/2</a></td><td>Return X^N.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ceil-1"></a>

### ceil/1 ###

<pre><code>
ceil(Value) -&gt; integer()
</code></pre>

<ul class="definitions"><li><code>Value = number()</code></li></ul>

Round a given number upwards towards to the nearest integer.

<a name="floor-1"></a>

### floor/1 ###

<pre><code>
floor(Value) -&gt; integer()
</code></pre>

<ul class="definitions"><li><code>Value = number()</code></li></ul>

Round a given number downwards towards the nearest integer.

<a name="mod-2"></a>

### mod/2 ###

<pre><code>
mod(X, Y) -&gt; integer()
</code></pre>

<ul class="definitions"><li><code>X = integer()</code></li><li><code>Y = integer()</code></li></ul>

Return X mod Y.

<a name="mod_pow-3"></a>

### mod_pow/3 ###

<pre><code>
mod_pow(Base, Exponent, Modulus) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Base = number()</code></li><li><code>Exponent = number()</code></li><li><code>Modulus = number()</code></li><li><code>Result = number()</code></li></ul>

Return B^E mod M.

<a name="pow-2"></a>

### pow/2 ###

<pre><code>
pow(X, N) -&gt; integer()
</code></pre>

<ul class="definitions"><li><code>X = integer()</code></li><li><code>N = integer()</code></li></ul>

Return X^N.

