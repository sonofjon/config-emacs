- C-x <left> and <right> should go to previous and next buffer based on
  recency
- What are the orderless completion algorithms should I consider?
- Orderless dispatcher keys seems non-intuitive, e.g. "<"" does not mean
  match from start, "=" does not mean exact match
- What is the point of having this, isn't file completion already included
  by default:

  (add-hook 'completion-at-point-functions #'cape-file)
