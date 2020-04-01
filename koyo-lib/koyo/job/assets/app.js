(function() {
  document.addEventListener("DOMContentLoaded", function() {
    document.querySelectorAll("[data-job-uri]").forEach(function(el) {
      el.addEventListener("click", function() {
        window.location.href = el.dataset.jobUri;
      });
    });
  });
})();
