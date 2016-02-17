

# Module onion_time #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Time Utility API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_unix_epoch-1">from_unix_epoch/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_unix_epoch-1">to_unix_epoch/1</a></td><td></td></tr><tr><td valign="top"><a href="#unix_epoch-0">unix_epoch/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_unix_epoch-1"></a>

### from_unix_epoch/1 ###

<pre><code>
from_unix_epoch(Timestamp::non_neg_integer()) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>
<br />

<a name="to_unix_epoch-1"></a>

### to_unix_epoch/1 ###

<pre><code>
to_unix_epoch(Now::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="unix_epoch-0"></a>

### unix_epoch/0 ###

<pre><code>
unix_epoch() -&gt; non_neg_integer()
</code></pre>
<br />

