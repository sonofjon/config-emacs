- C-x <left> and <right> should go to previous and next buffer based on
  recency
- What is the point of having this, isn't file completion already included
  by default:

  (add-hook 'completion-at-point-functions #'cape-file)
