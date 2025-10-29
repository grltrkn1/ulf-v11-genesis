#!/usr/bin/env python3
"""
simulate_hf_planning.py

ULF v11 Genesis Project — High-Fidelity Planning Simulation Engine
================================================================

This script is the "Crucible of Truth." It serves as the concrete, computational
"laboratory" where the SEI's abstract architectural hypotheses are tested.
It runs a controlled experiment comparing a Metric-Aware ATL planner (which
reasons about the deep NF3 structure) against a simplistic Symbolic ATL planner.

The output of this script is the 'Sum' — the empirical ground truth that drives
the entire self-evolutionary process of the ULF system.

Author: ULF Genesis Core Team
License: MIT
"""

import csv
import math
import os
import time
import argparse
from typing import List, Tuple, Callable, Dict

import numpy as np
import yaml

# Optional: for more structured instability fields
try:
    from scipy.ndimage import gaussian_filter
    SCIPY_AVAILABLE = True
except ImportError:
    SCIPY_AVAILABLE = False
    print("Warning: scipy not found. Instability field will be uniform random noise.")

# --------------------------------------------------------------------------
# 1. CORE MATHEMATICS: The hazard3D Function (Simplified)
# --------------------------------------------------------------------------

def sigmoid(x: float) -> float:
    """A numerically stable sigmoid function."""
    if x >= 0:
        return 1.0 / (1.0 + np.exp(-x))
    else:
        z = np.exp(x)
        return z / (1.0 + z)

def minkowski_metric(n: int) -> np.ndarray:
    """Returns a diagonal Minkowski-like metric M = diag(1, -1, -1, ...)."""
    diag = np.ones(n)
    if n > 1:
        diag[1:] = -1.0
    return np.diag(diag)

def hazard3D_from_cov(A: np.ndarray) -> float:
    """
    Given a real symmetric covariance matrix A, computes the dominant "timelike"
    eigenvalue of the generalized eigenproblem A*x = lambda*M*x.
    """
    n = A.shape[0]
    if n == 0:
        return 0.0
    M = minkowski_metric(n)

    try:
        from scipy import linalg
        # Use a solver that can handle generalized eigenproblems robustly
        vals, vecs = linalg.eig(A, M, right=True)
    except (ImportError, np.linalg.LinAlgError):
        # Fallback if scipy is not available or fails
        try:
            invM = np.diag(1.0 / np.diag(M))
            B = invM @ A
            vals, vecs = np.linalg.eig(B)
        except np.linalg.LinAlgError:
            vals, _ = np.linalg.eig(A)

    # Filter for "timelike" eigenvectors (v_hermitian * M * v > 0)
    eps = 1e-9
    timelike_eigenvalues = []
    for i in range(len(vals)):
        v = vecs[:, i]
        q = np.vdot(v, M @ v)
        if np.real(q) > eps:
            timelike_eigenvalues.append(np.real(vals[i]))

    if timelike_eigenvalues:
        return max(timelike_eigenvalues)
    else:
        return np.max(np.real(vals)) if len(vals) > 0 else 0.0

# --------------------------------------------------------------------------
# 2. ENVIRONMENT DEFINITION
# --------------------------------------------------------------------------

class GridEnv:
    """
    A 3D grid world with a hidden "instability" field that determines the
    true structural hazard at each location.
    """
    def __init__(self, config: Dict, rng: np.random.RandomState):
        self.gx, self.gy, self.gz = config['GRID_SIZE']
        self.rng = rng
        self.instability = self._generate_instability_field()

    def _generate_instability_field(self) -> np.ndarray:
        field = self.rng.rand(self.gx, self.gy, self.gz)
        if SCIPY_AVAILABLE:
            field = gaussian_filter(field, sigma=1.5)
            min_val, max_val = field.min(), field.max()
            if max_val > min_val:
                field = (field - min_val) / (max_val - min_val)
        return field

    def get_instability(self, loc: Tuple[int, int, int]) -> float:
        x, y, z = loc
        return float(self.instability[x, y, z])

    def in_bounds(self, loc: Tuple[int, int, int]) -> bool:
        x, y, z = loc
        return 0 <= x < self.gx and 0 <= y < self.gy and 0 <= z < self.gz

    def get_neighbors(self, loc: Tuple[int, int, int]) -> List[Tuple[int, int, int]]:
        x, y, z = loc
        neighbors = []
        for dx, dy, dz in [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]:
            n_loc = (x + dx, y + dy, z + dz)
            if self.in_bounds(n_loc):
                neighbors.append(n_loc)
        return neighbors

# --------------------------------------------------------------------------
# 3. NF3 TENSOR GENERATION AND ORACLE
# --------------------------------------------------------------------------

def get_local_covariance(n_vars: int, instability: float, rng: np.random.RandomState) -> np.ndarray:
    """Constructs a synthetic covariance matrix based on the hidden instability."""
    base_corr = 0.05 * rng.randn(n_vars, n_vars)
    sigma = np.eye(n_vars) * 0.5 + (base_corr + base_corr.T) / 2.0
    v_timelike = rng.randn(n_vars)
    v_timelike /= np.linalg.norm(v_timelike)
    amplification = 1.0 + 8.0 * instability**2
    sigma += (amplification - 1.0) * np.outer(v_timelike, v_timelike)
    return sigma

def get_oracle_hazard(config: Dict, n_vars: int, instability: float, rng: np.random.RandomState) -> float:
    """The ground truth hazard, computed from the local covariance structure."""
    sigma = get_local_covariance(n_vars, instability, rng)
    dominant_eigenvalue = hazard3D_from_cov(sigma)
    return sigmoid(config['ALPHA_ORACLE'] * (dominant_eigenvalue - 0.5))

# --------------------------------------------------------------------------
# 4. PLANNERS AND THEIR HAZARD FUNCTIONS
# --------------------------------------------------------------------------

def metric_hazard_function(config: Dict, loc: Tuple, env: GridEnv, n_vars: int, rng: np.random.RandomState) -> float:
    """The Metric ATL's hazard function, which correctly models the world."""
    instability = env.get_instability(loc)
    sigma = get_local_covariance(n_vars, instability, rng)
    dominant_eigenvalue = hazard3D_from_cov(sigma)
    return sigmoid(config['ALPHA_SIGMOID_METRIC'] * (dominant_eigenvalue - 0.5))

def symbolic_hazard_function(config: Dict, loc: Tuple, env: GridEnv) -> float:
    """The Symbolic ATL's hazard function, using a weak, uncorrelated heuristic."""
    x, y, z = loc
    max_dist = env.gx + env.gy + env.gz
    feature = (x + y + z) / max_dist
    return sigmoid(config['BETA_SYMBOLIC'] * (feature - 0.5))

def generate_candidate_paths(env: GridEnv, start: Tuple, goal: Tuple, k: int, max_len: int, rng: np.random.RandomState) -> List[List[Tuple]]:
    """Generates k candidate paths using a randomized greedy walk."""
    paths = []
    for _ in range(k):
        path = [start]
        current = start
        for _ in range(max_len - 1):
            if current == goal:
                break
            neighbors = env.get_neighbors(current)
            if not neighbors:
                break
            
            def distance(p1, p2):
                return sum([abs(c1 - c2) for c1, c2 in zip(p1, p2)])

            weights = np.array([np.exp(-0.8 * distance(n, goal)) for n in neighbors])
            probs = weights / weights.sum()
            next_loc = neighbors[rng.choice(len(neighbors), p=probs)]
            
            if next_loc in path:
                next_loc = random.choice(neighbors)
            path.append(next_loc)
            current = next_loc
        paths.append(path)
    return paths

def choose_best_path(candidates: List, hazard_fn: Callable, config: Dict, env: GridEnv) -> List[Tuple]:
    """Scores each path by its predicted expected reward and returns the best one."""
    best_score = -float('inf')
    best_path = []
    goal = (env.gx - 1, env.gy - 1, env.gz - 1)

    for path in candidates:
        prob_survive = 1.0
        for loc in path:
            predicted_hazard = hazard_fn(loc)
            prob_survive *= (1.0 - predicted_hazard)

        step_costs = config['STEP_COST'] * (len(path) - 1)
        reward = config['GOAL_REWARD'] if path[-1] == goal else 0
        
        predicted_expected_reward = prob_survive * (reward + step_costs) + (1.0 - prob_survive) * config['TRAP_PENALTY']
        
        if predicted_expected_reward > best_score:
            best_score = predicted_expected_reward
            best_path = path
            
    return best_path if best_path else (candidates[0] if candidates else [])

# --------------------------------------------------------------------------
# 5. MAIN EXPERIMENT LOOP
# --------------------------------------------------------------------------

def run_experiment(config: Dict):
    """The main function to run the entire simulation and log results."""
    print(f"Starting experiment with {config['N_EPISODES']} episodes...")
    n_vars = config['NF3_D'] ** 3
    results = []

    for seed in range(config['N_EPISODES']):
        episode_rng = np.random.RandomState(config['RANDOM_SEED_BASE'] + seed)
        env = GridEnv(config, episode_rng)
        start = (0, 0, 0)
        goal = (env.gx - 1, env.gy - 1, env.gz - 1)
        
        candidates = generate_candidate_paths(env, start, goal, config['CANDIDATE_PATHS'], config['MAX_PATH_LEN'], episode_rng)

        # Planners make their choices
        metric_path = choose_best_path(candidates, lambda loc: metric_hazard_function(config, loc, env, n_vars, episode_rng), config, env)
        symbolic_path = choose_best_path(candidates, lambda loc: symbolic_hazard_function(config, loc, env), config, env)

        # Simulate and evaluate outcomes
        reward_metric, survived_metric = simulate_path(metric_path, config, env, n_vars, episode_rng)
        reward_symbolic, survived_symbolic = simulate_path(symbolic_path, config, env, n_vars, episode_rng)

        results.append({
            "seed": seed,
            "reward_metric": reward_metric,
            "reward_symbolic": reward_symbolic,
            "survived_metric": int(survived_metric),
            "survived_symbolic": int(survived_symbolic),
        })

        if seed % 20 == 0 or seed == config['N_EPISODES'] - 1:
            print(f"Episode {seed}/{config['N_EPISODES']}: Metric Reward={reward_metric:.2f}, Symbolic Reward={reward_symbolic:.2f}")

    # Save results to CSV
    df = pd.DataFrame(results)
    df.to_csv("results.csv", index=False)
    print(f"\nExperiment finished. Results saved to results.csv")

def simulate_path(path: List, config: Dict, env: GridEnv, n_vars: int, rng: np.random.RandomState) -> Tuple[float, bool]:
    """Simulates traversing a path, returning the final reward and survival status."""
    for loc in path:
        instability = env.get_instability(loc)
        true_hazard = get_oracle_hazard(config, n_vars, instability, rng)
        if rng.rand() < true_hazard:
            return config['TRAP_PENALTY'], False # Trap triggered

    step_costs = config['STEP_COST'] * (len(path) - 1)
    goal = (env.gx - 1, env.gy - 1, env.gz - 1)
    reward = config['GOAL_REWARD'] if path and path[-1] == goal else 0
    return reward + step_costs, True

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="ULF v11 - Metric vs. Symbolic Planner Simulation")
    parser.add_argument("--config", default="config/experiment.yaml", help="Path to experiment configuration file")
    args = parser.parse_args()

    try:
        with open(args.config, 'r') as f:
            config = yaml.safe_load(f)
    except FileNotFoundError:
        print(f"Warning: Config file '{args.config}' not found. Using internal defaults.")
        config = {
            'N_EPISODES': 200, 'RANDOM_SEED_BASE': 42, 'GRID_SIZE': [8,8,4],
            'NF3_D': 3, 'MAX_PATH_LEN': 35, 'CANDIDATE_PATHS': 50,
            'GOAL_REWARD': 100.0, 'TRAP_PENALTY': -100.0, 'STEP_COST': -1.0,
            'ALPHA_ORACLE': 2.5, 'ALPHA_SIGMOID_METRIC': 1.5, 'BETA_SYMBOLIC': 3.0
        }
    
    print("--- ULF v11 Genesis: Experiment Engine ---")
    print(f"Configuration loaded from: {args.config if os.path.exists(args.config) else 'Internal Defaults'}")
    
    t0 = time.time()
    run_experiment(config)
    t1 = time.time()
    
    print(f"Total simulation time: {t1 - t0:.2f} seconds")
