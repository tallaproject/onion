

# Module onion_random #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Utilities for working with random data.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bytes-1">bytes/1</a></td><td>Return N random bytes.</td></tr><tr><td valign="top"><a href="#bytes_unsigned-1">bytes_unsigned/1</a></td><td>Return N random bytes as an unsigned integer.</td></tr><tr><td valign="top"><a href="#coin_toss-0">coin_toss/0</a></td><td>Toss a coin.</td></tr><tr><td valign="top"><a href="#hostname-4">hostname/4</a></td><td>Return random hostname.</td></tr><tr><td valign="top"><a href="#pick-1">pick/1</a></td><td>Return a random element from a given list.</td></tr><tr><td valign="top"><a href="#time_range-2">time_range/2</a></td><td>Return an UNIX epoch within Start and End.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bytes-1"></a>

### bytes/1 ###

<pre><code>
bytes(N) -&gt; RandomBytes
</code></pre>

<ul class="definitions"><li><code>N = non_neg_integer()</code></li><li><code>RandomBytes = binary()</code></li></ul>

Return N random bytes.

<a name="bytes_unsigned-1"></a>

### bytes_unsigned/1 ###

<pre><code>
bytes_unsigned(N) -&gt; RandomUnsigned
</code></pre>

<ul class="definitions"><li><code>N = non_neg_integer()</code></li><li><code>RandomUnsigned = non_neg_integer()</code></li></ul>

Return N random bytes as an unsigned integer.

<a name="coin_toss-0"></a>

### coin_toss/0 ###

<pre><code>
coin_toss() -&gt; head | tail
</code></pre>
<br />

Toss a coin.

<a name="hostname-4"></a>

### hostname/4 ###

<pre><code>
hostname(Min, Max, Prefix, Suffix) -&gt; <a href="inet.md#type-hostname">inet:hostname()</a>
</code></pre>

<ul class="definitions"><li><code>Min = non_neg_integer()</code></li><li><code>Max = non_neg_integer()</code></li><li><code>Prefix = string()</code></li><li><code>Suffix = string()</code></li></ul>

Return random hostname.

<a name="pick-1"></a>

### pick/1 ###

<pre><code>
pick(List) -&gt; Element
</code></pre>

<ul class="definitions"><li><code>Element = term()</code></li><li><code>List = [Element]</code></li></ul>

Return a random element from a given list.

<a name="time_range-2"></a>

### time_range/2 ###

<pre><code>
time_range(Min, Max) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Min = non_neg_integer()</code></li><li><code>Max = non_neg_integer()</code></li><li><code>Result = non_neg_integer()</code></li></ul>

Return an UNIX epoch within Start and End.

