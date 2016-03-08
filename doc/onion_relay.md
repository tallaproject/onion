

# Module onion_relay #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Relay Utility Functions.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#valid_nickname-1">valid_nickname/1</a></td><td></td></tr><tr><td valign="top"><a href="#valid_nickname_or_relay_fingerprint-1">valid_nickname_or_relay_fingerprint/1</a></td><td></td></tr><tr><td valign="top"><a href="#valid_relay_fingerprint-1">valid_relay_fingerprint/1</a></td><td></td></tr><tr><td valign="top"><a href="#verify_digest-2">verify_digest/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="valid_nickname-1"></a>

### valid_nickname/1 ###

<pre><code>
valid_nickname(Nickname) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Nickname = string()</code></li></ul>

<a name="valid_nickname_or_relay_fingerprint-1"></a>

### valid_nickname_or_relay_fingerprint/1 ###

<pre><code>
valid_nickname_or_relay_fingerprint(String) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>String = string()</code></li></ul>

<a name="valid_relay_fingerprint-1"></a>

### valid_relay_fingerprint/1 ###

<pre><code>
valid_relay_fingerprint(Fingerprint) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Fingerprint = string()</code></li></ul>

<a name="verify_digest-2"></a>

### verify_digest/2 ###

<pre><code>
verify_digest(Context, Data) -&gt; {ok, NewContext} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Context = term()</code></li><li><code>NewContext = term()</code></li><li><code>Data = binary()</code></li><li><code>Reason = term()</code></li></ul>

