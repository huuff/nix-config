(use-package neuron
  :mode ("\\.md\\'" . neuron-mode)
  :hook (neuron-mode . company-neuron-setup)
  :hook (neuron-mode . texfrag-mode)
  :config
)

; use something better, like detecting .neuron
;(add-to-list 'auto-mode-alist '("\\.md\\'" . neuron-mode))
