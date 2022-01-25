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
Rcpp::List align_spectras(const Rcpp::DataFrame l_spectra, 
		const Rcpp::ListOf<Rcpp::DataFrame> q_spectras, 
		const float min_deviation_mz = 0.05, 
		const float min_deviation_abd = 25.0) {

	// initialize m/z, abd, weights & order in library spectra
    std::vector<float> l_mz = Rcpp::as<std::vector<float> >(l_spectra["mz"]);
	std::vector<float> l_abd = Rcpp::as<std::vector<float> >(l_spectra["abd"]);
    std::vector<float> l_weight = get_weights(l_abd);
	// container of same length than q_mz, will contain all weights for each feature
	std::vector<std::size_t> l_order = get_order_desc(l_abd);
	std::vector<std::size_t> l_idx(l_mz.size(), 0);
	std::iota(l_idx.begin(), l_idx.end(), 1);
	
	// initialize m/z, abd & order in query spectra
    std::vector<std::vector<float> > q_mz(q_spectras.size());
    std::vector<std::vector<float> > q_abd(q_spectras.size());
	std::vector<std::vector<std::size_t> > q_order(q_spectras.size());
	// also for each theo will create a vector of same length than the number of peaks in obs
		// will contain for each theoric feature at `i` the index of the corresponding observed feature
	std::vector<std::vector<size_t> > q_idx(q_mz.size());
	std::vector<size_t> idx_2 (l_mz.size(), NA_INTEGER);
	
    float min_deviation_abd_obs;
    float deviation_mz_obs;
    float deviation_abd_obs;
	
    Rcpp::List idx(q_mz.size());
	std::vector<float> score(q_mz.size(), 0);
	std::vector<float> npeak(q_mz.size(), 0);
	std::vector<float> deviation_mz(q_mz.size());
	
	std::vector<std::size_t>::iterator q_it;
	std::vector<std::size_t>::iterator l_it;
	std::vector<std::size_t>::iterator last_it;
	bool match;
	
    // for each query spectra
    for (std::size_t i = 0; i < q_order.size(); i++) {
        // load mz, abd & order & idx
        q_mz[i] = Rcpp::as<std::vector<float> >(q_spectras[i]["mz"]);
        q_abd[i] = Rcpp::as<std::vector<float> >(q_spectras[i]["abd"]);
        q_order[i] = get_order_desc(q_abd[i]);
        q_idx[i] = idx_2;
        // for each library feature
        for (l_it = l_order.begin(); l_it != l_order.end(); l_it++) {
            min_deviation_abd_obs = std::numeric_limits<float>::infinity();
            match = false;
            // for each query feature search the corresponding feature
            if (q_order[i].size() > 0) {
                for (q_it = q_order[i].begin(); q_it != q_order[i].end(); q_it++) {
                    deviation_abd_obs = q_abd[i][*q_it] - l_abd[*l_it];
                    deviation_mz_obs = fabs(q_mz[i][*q_it] - l_mz[*l_it]);
                    // only accept observed peaks where the abundance is over a minimum
                        // an observed peak can be much higher than the theoretical (hided by another peak ?)
                    if (deviation_abd_obs >= -min_deviation_abd && 
                            fabs(deviation_abd_obs) < min_deviation_abd_obs && 
                            deviation_mz_obs < min_deviation_mz) {
                        last_it = q_it;
                        match = true;
                        min_deviation_abd_obs = fabs(deviation_abd_obs);
                    }
                }
            }
            // if no match it will stop directly
            if (match) {
                min_deviation_abd_obs = min_deviation_abd_obs > min_deviation_abd ? min_deviation_abd : min_deviation_abd_obs;
                score[i] += (1 - min_deviation_abd_obs / min_deviation_abd) * 
                    l_weight[*l_it];
                // mz deviation is not absolute
                deviation_mz[i] += q_mz[i][*last_it] - l_mz[*l_it];
                npeak[i]++;
                q_idx[i][*l_it] = *last_it + 1;
                q_order[i].erase(last_it); 
            } else break;
        }
    
        l_idx.resize(l_mz.size());
        if (q_order[i].size() > 0) {
            for (q_it = q_order[i].begin(); q_it != q_order[i].end(); q_it++) {
                l_idx.push_back(NA_INTEGER);
                q_idx[i].push_back(*q_it + 1);
            }
        }
        idx[i] = Rcpp::DataFrame::create( 
            Rcpp::Named("q_id") = q_idx[i], 
            Rcpp::Named("l_id") = l_idx);
        score[i] = 100.0 * score[i];
        deviation_mz[i] = deviation_mz[i] / npeak[i];
    }

	return Rcpp::List::create(
		Rcpp::Named("idx") = idx,
		Rcpp::Named("score") = score, 
		Rcpp::Named("deviation_mz") = deviation_mz, 
		Rcpp::Named("npeak") = npeak
	);
}