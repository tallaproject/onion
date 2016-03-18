

# Module onion_time #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Time Utility API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#epoch-0">epoch/0</a></td><td>Get current UNIX epoch.</td></tr><tr><td valign="top"><a href="#from_epoch-1">from_epoch/1</a></td><td>Convert UNIX epoch to calendar:datetime().</td></tr><tr><td valign="top"><a href="#to_epoch-1">to_epoch/1</a></td><td>Convert calendar:datetime() to UNIX epoch.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="epoch-0"></a>

### epoch/0 ###

<pre><code>
epoch() -&gt; non_neg_integer()
</code></pre>
<br />

Get current UNIX epoch.

<a name="from_epoch-1"></a>

### from_epoch/1 ###

<pre><code>
from_epoch(Timestamp::non_neg_integer()) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

Convert UNIX epoch to calendar:datetime().

<a name="to_epoch-1"></a>

### to_epoch/1 ###

<pre><code>
to_epoch(Now::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Convert calendar:datetime() to UNIX epoch.

