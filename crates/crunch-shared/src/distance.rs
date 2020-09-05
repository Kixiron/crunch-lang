use core::{cmp, ptr::NonNull};

pub fn levenshtein_distance(left: &str, right: &str) -> usize {
    let mut buf = Vec::with_capacity(right.len());
    levenshtein_distance_inner(left, right, &mut buf)
}

fn levenshtein_distance_inner(left: &str, right: &str, distances: &mut Vec<usize>) -> usize {
    if left.is_empty() {
        return left.chars().count();
    } else if right.is_empty() {
        return right.chars().count();
    }

    distances.clear();
    distances.extend(0..=right.len());
    let mut t_last = 0;

    for (i, left_char) in left.chars().enumerate() {
        let mut current = i;
        distances[0] = current + 1;

        for (j, right_char) in right.chars().enumerate() {
            let next = distances[j + 1];

            if left_char == right_char {
                distances[j + 1] = current;
            } else {
                distances[j + 1] = cmp::min(current, next);
                distances[j + 1] = cmp::min(distances[j + 1], distances[j]) + 1;
            }

            current = next;
            t_last = j;
        }
    }

    distances[t_last + 1]
}

pub fn find_best_match<'a, I>(
    needle: &str,
    haystack: I,
    max_distance: Option<usize>,
    word_mode: WordMode,
) -> Option<&'a str>
where
    I: Iterator<Item = &'a str> + Clone + 'a,
{
    let max_distance = max_distance.unwrap_or_else(|| cmp::max(needle.len(), 3) / 3);

    let mut buf = Vec::with_capacity(needle.len());
    let (case_insensitive_match, levenshtein_match) = haystack
        .clone()
        .filter_map(|name| {
            let dist = levenshtein_distance_inner(needle, name, &mut buf);

            if dist <= max_distance {
                Some((name, dist))
            } else {
                None
            }
        })
        .fold(
            (None, None),
            |(prev_match, prev_dist), (candidate, dist)| {
                let case_insensitive_match = if candidate.to_uppercase() == needle.to_uppercase() {
                    Some(candidate)
                } else {
                    prev_match
                };

                let levenshtein = match prev_dist {
                    None => Some((candidate, dist)),
                    Some((c, d)) => Some(if dist < d { (candidate, dist) } else { (c, d) }),
                };

                (case_insensitive_match, levenshtein)
            },
        );

    // Priority of matches:
    // 1. Exact case insensitive match
    // 2. Levenshtein distance match
    // 3. Sorted word match
    if let Some(candidate) = case_insensitive_match {
        Some(candidate)
    } else if levenshtein_match.is_some() {
        levenshtein_match.map(|(candidate, _)| candidate)
    } else {
        find_match_by_sorted_words(needle, haystack, word_mode)
    }
}

fn find_match_by_sorted_words<'a, I>(
    needle: &str,
    haystack: I,
    word_mode: WordMode,
) -> Option<&'a str>
where
    I: Iterator<Item = &'a str> + Clone + 'a,
{
    let mut buf = Vec::with_capacity(needle.len() / 3);
    let needle_words = sort_by_words(needle, word_mode, &mut buf);

    haystack.fold(None, |result, candidate| {
        if sort_by_words(candidate, word_mode, &mut buf) == needle_words {
            Some(candidate)
        } else {
            result
        }
    })
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum WordMode {
    SnakeCase,
    KebabCase,
}

fn sort_by_words<'a>(name: &'a str, word_mode: WordMode, words: &mut Vec<NonNull<str>>) -> String {
    // Split each name into words
    match word_mode {
        WordMode::SnakeCase => words.extend(name.split('_').map(NonNull::from)),
        WordMode::KebabCase => words.extend(name.split('-').map(NonNull::from)),
    }
    // Sort the split words
    words.sort();

    // Rebuild the string from sorted words
    let mut result = String::with_capacity(name.len());
    let mut iter = words.iter();
    let last = iter.next_back();

    for word in words.iter() {
        // Safety: The pointers will be valid, we just made them
        result.push_str(unsafe { word.as_ref() });
        result.push('_');
    }
    if let Some(last) = last {
        // Safety: The pointers will be valid, we just made them
        result.push_str(unsafe { last.as_ref() });
    }

    // Clean up the pointers in the buffer
    words.clear();

    result
}
