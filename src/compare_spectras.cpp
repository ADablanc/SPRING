#include "Rcpp.h"
#include <vector>
#include <algorithm>
#include <math.h>
#include <numeric>
#include <limits>

# //[[Rcpp::plugins(cpp11)]]
std::vector<std::size_t> get_order_desc (const std::vector<float>& vec) {
	std::vector<size_t> order(vec.size());
    std::iota(order.begin(), order.end(), 0);
    std::sort(order.begin(), order.end(), [&](size_t a, size_t b) {
        return vec[a] > vec[b];
    });
    return order;
}

void convert_to_abd (std::vector<float>& ints) {
	float max_int = *std::max_element(ints.begin(), ints.end());
	for (std::vector<float>::iterator it = ints.begin(); it != ints.end(); it++) 
		*it = *it / max_int * 100.0;
}

std::vector<float> get_weights (const std::vector<float>& abd) {
	std::vector<float> weights(abd.size());
	float sum_abd = std::accumulate(abd.begin(), abd.end(), 0.0);
	for (std::size_t i = 0; i < abd.size(); i++) 
		weights[i] = abd[i] / sum_abd;
	return weights;
}

// return for each library spectra a vector of same length than the query spectra
	// with the id of the library feature which correspond
// [[Rcpp::export]]
Rcpp::List align_spectras(const Rcpp::DataFrame q_spectra, 
		const Rcpp::ListOf<Rcpp::DataFrame> l_spectras, 
		const float min_deviation_mz = 0.05, 
		const float min_deviation_abd = 25.0) {

	// initialize m/z, abd, weights & order in obs
    std::vector<float> q_mz = Rcpp::as<std::vector<float> >(q_spectra["mz"]);
	std::vector<float> q_abd(q_mz.size());
	if (q_spectra.containsElementNamed("abd")) 
		q_abd = Rcpp::as<std::vector<float> >(q_spectra["abd"]);
	else {
		q_abd = Rcpp::as<std::vector<float> >(q_spectra["int"]);
		convert_to_abd(q_abd);
	}
	// container of same length than q_mz, will contain all weights for each feature
	std::vector<float> q_score_weights = get_weights(q_abd);
	std::vector<std::size_t> q_order = get_order_desc(q_abd);
	std::vector<std::size_t> q_idx(q_mz.size(), 0);
	std::iota(q_idx.begin(), q_idx.end(), 1);
	
	// initialize m/z, abd & order in theo
    std::vector<std::vector<float> > l_mz(l_spectras.size());
    std::vector<std::vector<float> > l_abd(l_spectras.size());
	std::vector<std::vector<std::size_t> > l_order(l_spectras.size());
	// also create weights	
	std::vector<std::vector<float> > l_score_weights(l_mz.size());
	// also for each theo will create a vector of same length than the number of peaks in obs
		// will contain for each obs feature at `i` the index of the corresponding theoric feature
	std::vector<std::vector<size_t> > l_idx(l_mz.size());
	std::vector<size_t> idx_2 (q_mz.size(), NA_INTEGER);
	unsigned int i;
	for (i = 0; i < l_spectras.size(); i++) {
		l_mz[i] = Rcpp::as<std::vector<float> >(l_spectras[i]["mz"]);
		if (l_spectras[i].containsElementNamed("abd")) 
			l_abd[i] = Rcpp::as<std::vector<float> >(l_spectras[i]["abd"]);
		else {
			l_abd[i] = Rcpp::as<std::vector<float> >(l_spectras[i]["int"]);
			convert_to_abd(l_abd[i]);
		}
		l_order[i] = get_order_desc(l_abd[i]);
		l_score_weights[i] = get_weights(l_abd[i]);
		l_idx[i] = idx_2;
	}
	
	float deviation_mz_obs;
    float deviation_abd_obs;
	
	std::vector<float> score(l_mz.size(), 0.0);
	std::vector<float> npeak(l_mz.size(), 0);
	std::vector<float> deviation_mz(l_mz.size());
	
	std::vector<std::size_t>::iterator q_it;
	std::vector<std::size_t>::iterator l_it;
	// for each observed feature
    for (q_it = q_order.begin(); q_it != q_order.end(); q_it++) {
		// for each theoric
        for (std::size_t i = 0; i < l_order.size(); i++) {
			// search neightboured m/z
			if (l_order[i].size() > 0) {
                l_it = l_order[i].begin();
				deviation_abd_obs = q_abd[*q_it] - l_abd[i][*l_it];
				deviation_mz_obs = fabs(q_mz[*q_it] - l_mz[i][*l_it]);
				// only accept observed peaks where the abundance is over a minimum
					// an observed peak can be much higher than the theoretical (hided by another peak ?)
				if (deviation_abd_obs >= -min_deviation_abd && deviation_mz_obs < min_deviation_mz) {
					deviation_abd_obs = deviation_abd_obs > min_deviation_abd ? min_deviation_abd : fabs(deviation_abd_obs);
					score[i] += deviation_abd_obs / min_deviation_abd * 
						l_score_weights[i][*l_it];
					// mz deviation is not absolute
					deviation_mz[i] += q_mz[*q_it] - l_mz[i][*l_it];
					npeak[i]++;
					l_idx[i][*q_it] = *l_it + 1;
					l_order[i].erase(l_it);
				}
            }
        }
    }
	
	Rcpp::List idx(l_mz.size());
	for (i = 0; i < l_idx.size(); i++) {
		q_idx.resize(q_mz.size());
		if (l_order[i].size() > 0) {
			for (std::vector<std::size_t>::iterator l_it = l_order[i].begin(); 
					l_it != l_order[i].end(); l_it++) {
				score[i] += l_score_weights[i][*l_it];
				q_idx.push_back(NA_INTEGER);
				l_idx[i].push_back(*l_it + 1);
			}
		}
		
		idx[i] = Rcpp::DataFrame::create( 
			Rcpp::Named("q_id") = q_idx, 
			Rcpp::Named("l_id") = l_idx[i]);
		score[i] = 100.0 * (1.0 - score[i]);
		deviation_mz[i] = deviation_mz[i] / npeak[i];
	}
	return Rcpp::List::create(
		Rcpp::Named("idx") = idx,
		Rcpp::Named("score") = score, 
		Rcpp::Named("deviation_mz") = deviation_mz, 
		Rcpp::Named("npeak") = npeak
	);
}