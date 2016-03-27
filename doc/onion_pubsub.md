

# Module onion_pubsub #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Pub/Sub API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send a message to a given topic.</td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td>Subscribe to a given topic.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Topic, Message) -&gt; {pid(), Topic, Message}
</code></pre>

<ul class="definitions"><li><code>Topic = term()</code></li><li><code>Message = term()</code></li></ul>

Send a message to a given topic.

<a name="subscribe-1"></a>

### subscribe/1 ###

<pre><code>
subscribe(Topic) -&gt; true
</code></pre>

<ul class="definitions"><li><code>Topic = term()</code></li></ul>

Subscribe to a given topic.

