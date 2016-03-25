

# Module onion_geoip #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

GeoIP API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_ipv4_file-1">parse_ipv4_file/1</a></td><td>Parse a GeoIP file containing IPv4 addresses.</td></tr><tr><td valign="top"><a href="#parse_ipv6_file-1">parse_ipv6_file/1</a></td><td>Parse a GeoIP file containing IPv6 addresses.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_ipv4_file-1"></a>

### parse_ipv4_file/1 ###

<pre><code>
parse_ipv4_file(Filename) -&gt; {ok, [{<a href="inet.md#type-ip4_address">inet:ip4_address()</a>, <a href="inet.md#type-ip4_address">inet:ip4_address()</a>, binary()}], Digest} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Filename = <a href="file.md#type-filename">file:filename()</a></code></li><li><code>Digest = binary()</code></li><li><code>Reason = term()</code></li></ul>

Parse a GeoIP file containing IPv4 addresses.

<a name="parse_ipv6_file-1"></a>

### parse_ipv6_file/1 ###

<pre><code>
parse_ipv6_file(Filename) -&gt; {ok, [{<a href="inet.md#type-ip6_address">inet:ip6_address()</a>, <a href="inet.md#type-ip6_address">inet:ip6_address()</a>, binary()}], Digest} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Filename = <a href="file.md#type-filename">file:filename()</a></code></li><li><code>Digest = binary()</code></li><li><code>Reason = term()</code></li></ul>

Parse a GeoIP file containing IPv6 addresses.

