{:user {:dependencies [[pjstadig/humane-test-output "0.7.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[cider/cider-nrepl "0.8.2"]
                  [refactor-nrepl "0.2.2"]
                  [jonase/eastwood "0.2.1"]
                  [lein-kibit "0.0.8"]] }}
