import numpy as np
import jax
import jax.numpy as jnp
import numpyro
import numpyro.distributions as dist
from scipy.interpolate import BSpline
from numpyro.infer import MCMC, NUTS

def construct_P_foi(beta, gamma, rho):
    P = jnp.eye(4)
    # beta - force of infection
    P = P.at[0, 0].add(-beta)
    P = P.at[1, 0].add(beta)
    # gamma - recovery rate
    P = P.at[1, 1].add(-gamma)
    P = P.at[0, 1].add(gamma)
    # rho - treatment
    P = P.at[1, 1].add(-rho)
    P = P.at[2, 1].add(rho)
    # stay in treatment for one step
    P = P.at[2, 2].add(-1)
    P = P.at[3, 2].add(1)
    # recover from treatment
    P = P.at[3, 3].add(-1)
    P = P.at[0, 3].add(1)
    return P

def construct_basis(knot_vals, n_steps):
    n_knots = len(knot_vals) + 4
    step = n_steps / (n_knots - 1 - 6)
    knots = np.linspace(0 - 3 * step, n_steps + 3 * step, n_knots)
    x = np.arange(1, n_steps + 1)
    basis = BSpline(knots, np.eye(len(knots) - 4), 3)(x)
    return jnp.array(basis)

def run_sim(knot_vals, gamma, rhos, basis, n_steps):
    betas = jax.nn.sigmoid(basis @ knot_vals)
    # Initial state
    P = construct_P_foi(betas[0], gamma, rhos[0])
    # Use left eigenvector for stationary distribution
    eigvals, eigvecs = jnp.linalg.eig(P)
    ev = jnp.real(eigvecs[:, 0])
    x0 = jnp.abs(ev / jnp.sum(ev))
    x_all = jnp.zeros((len(x0), n_steps + 1))
    x_all = x_all.at[:, 0].set(x0)
    x = x0
    new_inf = jnp.zeros(n_steps + 1)
    new_inf = new_inf.at[0].set(P[1, 0] * x[0])
    for i in range(n_steps):
        P = construct_P_foi(betas[i + 1], gamma, rhos[i])
        new_inf = new_inf.at[i + 1].set(P[1, 0] * x[0])
        x = P @ x
        x_all = x_all.at[:, i + 1].set(x)
    return x_all, new_inf

# Example NumPyro model
def outbreak_model(obs=None, n_steps=100, n_knots=10, gamma=0.1, rhos=None, basis=None):
    # Prior for knot values
    knot_vals = numpyro.sample("knot_vals", dist.Normal(0, 1).expand([n_knots]))
    # Simulate
    x_all, new_inf = run_sim(knot_vals, gamma, rhos, basis, n_steps)
    # Likelihood (example: Poisson)
    if obs is not None:
        numpyro.sample("obs", dist.Poisson(new_inf[1:]), obs=obs)

# Example usage
if __name__ == "__main__":
    n_steps = 100
    n_knots = 10
    gamma = 0.1
    rhos = jnp.ones(n_steps + 1) * 0.05
    knot_vals = np.random.normal(0, 1, n_knots)
    basis = construct_basis(knot_vals, n_steps)
    # Fake data for demonstration
    _, new_inf = run_sim(jnp.array(knot_vals), gamma, rhos, basis, n_steps)
    obs = np.random.poisson(new_inf[1:])

    nuts_kernel = NUTS(outbreak_model)
    mcmc = MCMC(nuts_kernel, num_warmup=100, num_samples=200)
    mcmc.run(jax.random.PRNGKey(0), obs=obs, n_steps=n_steps, n_knots=n_knots, gamma=gamma, rhos=rhos, basis=basis)
    print(mcmc.get_samples())