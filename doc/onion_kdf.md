

# Module onion_kdf #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Key Derivation Functions used in Tor.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
For information about the HKDF function, see RFC 5869.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#hkdf-4">hkdf/4</a></td><td>HMAC-based Extract-and-Expand Key Derivation Function.</td></tr><tr><td valign="top"><a href="#kdf_tor-2">kdf_tor/2</a></td><td>KDF-Tor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="hkdf-4"></a>

### hkdf/4 ###

<pre><code>
hkdf(Key, Salt, Info, Length) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Key = iodata()</code></li><li><code>Salt = iodata()</code></li><li><code>Info = iodata()</code></li><li><code>Length = pos_integer()</code></li></ul>

HMAC-based Extract-and-Expand Key Derivation Function.

For more information, see RFC 5869.

<a name="kdf_tor-2"></a>

### kdf_tor/2 ###

<pre><code>
kdf_tor(Key, Length) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Key = binary()</code></li><li><code>Length = pos_integer()</code></li></ul>

KDF-Tor

For more information, see tor-spec.txt section 5.2.1.

