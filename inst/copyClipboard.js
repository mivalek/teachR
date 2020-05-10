
<script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.4/clipboard.min.js"></script>


<script>
// Tooltip

$('button').tooltip({
  trigger: 'click',
  placement: 'bottom'
});

function setTooltip(btn,message) {
  $(btn).tooltip('destroy')
    .attr('data-original-title', message)
    .tooltip('show');
}

function hideTooltip(btn) {
  setTimeout(function() {
    $(btn).tooltip('destroy');
  }, 500);
}

// Clipboard

var clipboard = new ClipboardJS('button');

clipboard.on('success', function(e) {
  e.clearSelection();
  setTooltip(e.trigger, 'Markdown copied');
  hideTooltip(e.trigger);
});

clipboard.on('error', function(e) {
  e.clearSelection();
  setTooltip(e.trigger, 'Try again');
  hideTooltip(e.trigger);
});

</script>
