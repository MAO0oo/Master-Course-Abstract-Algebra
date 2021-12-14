#include <bits/stdc++.h>

typedef unsigned int u32;
typedef long long i64;

struct frac {
	i64 p, q;
	frac(i64 p = 0, i64 q = 1): p(p), q(q) { }

	void fix() {
		if (q < 0) {
			p = -p;
			q = -q;
		}
		
		i64 d = std::gcd(std::abs(p), q);
		p /= d;
		q /= d;
	}

	frac operator-() const {
		return frac(-p, q);
	}

	frac operator~() const {
		assert(p);

		if (p < 0) {
			return frac(-q, -p);
		}

		return frac(q, p);
	}
};

std::ostream& operator<<(std::ostream& os, const frac x) {
	os << x.p;

	if (x.q != 1) {
		os << '/' << x.q;
	}

	return os;
}

#define decl(tp, op, lhs, rhs) tp operator op (const tp& lhs, const tp& rhs)

decl(frac, +, x, y) {
	frac z(x.p * y.q + y.p * x.q, x.q * y.q);
	z.fix();
	return z;
}

decl(frac, -, x, y) {
	frac z(x.p * y.q - y.p * x.q, x.q * y.q);
	z.fix();
	return z;
}

decl(frac, *, x, y) {
	frac z(x.p * y.p, x.q * y.q);
	z.fix();
	return z;
}

decl(frac, /, x, y) {
	frac z(x.p * y.q, x.q * y.p);
	z.fix();
	return z;
}

void withx(std::ostream& os, const frac& a, u32 n, char x = 'u') {
	if (!a.p) {
		os << '0';
		return;
	}

	if (!n) {
		os << a;
		return;
	}

	if (a.p != 1 || a.q != 1) {
		os << a << ' ';
	}

	os << x;

	if (n != 1u) {
		os << '^' << n;
	}
}

typedef std::vector<frac> poly;

std::ostream& operator<<(std::ostream& os, const poly& f) {
	if (f.empty()) {
		return os << '0';
	}

	withx(os, f.back(), f.size() - 1);

	for (u32 i = f.size() - 2; ~i; --i) {
		if (f[i].p < 0) {
			os << " - ";
			withx(os, -f[i], i);
		}

		if (f[i].p > 0) {
			os << " + ";
			withx(os, f[i], i);
		}
	}

	return os;
}

decl(poly, +, f, g) {
	u32 n = std::max(f.size(), g.size()), m = std::min(f.size(), g.size());
	poly h(n);

	for (u32 i = 0; i != m; ++i) {
		h[i] = f[i] + g[i];
	}

	for (u32 i = m; i != n; ++i) {
		if (f.size() == m) {
			h[i] = g[i];
		}
		else {
			h[i] = f[i];
		}
	}

	while (!h.empty() && !h.back().p) {
		h.pop_back();
	}

	return h;
}

decl(poly, -, f, g) {
	u32 n = std::max(f.size(), g.size()), m = std::min(f.size(), g.size());
	poly h(n);

	for (u32 i = 0; i != m; ++i) {
		h[i] = f[i] - g[i];
	}

	for (u32 i = m; i != n; ++i) {
		if (f.size() == m) {
			h[i] = -g[i];
		}
		else {
			h[i] = f[i];
		}
	}

	while (!h.empty() && !h.back().p) {
		h.pop_back();
	}

	return h;
}

std::pair<poly, poly> divide(poly f, poly g) {
	std::reverse(g.begin(), g.end());
	assert(!g.empty() && g[0].p);

	if (g.size() == 1u) {
		for (auto& x : f) {
			x = x / g[0];
		}

		return std::make_pair(f, poly());
	}

	if (f.size() < g.size()) {
		return std::make_pair(poly(), f);                     
	}

	poly h;

	for (u32 i = f.size() - 1; i + 1 >= g.size(); --i) {
		h.push_back(f[i] / g[0]);
		f[i] = 0;

		for (u32 j = 1; j != g.size(); ++j) {
			f[i - j] = f[i - j] - g[j] * h.back();
		}
	}

	while (!f.empty() && !f.back().p) {
		f.pop_back();
	}

	reverse(h.begin(), h.end());
	return std::make_pair(h, f);
}

poly operator-(const poly& f) {
	poly g(f);

	for (auto& x : g) {
		x = -x;
	}

	return g;
}

poly mod;

poly fix(const poly& f) {
	return divide(f, mod).second;
}

decl(poly, *, f, g) {
	if (f.empty()) {
		return fix(g);
	}

	if (g.empty()) {
		return fix(f);
	}

	u32 n = f.size(), m = g.size();
	poly h(n + m - 1);

	for (u32 i = 0; i != n; ++i) {
		for (u32 j = 0; j != m; ++j) {
			h[i + j] = h[i + j] + f[i] * g[j];
		}
	}

	return fix(h);
}

poly operator~(poly f) {
	f = fix(f);
	assert(!f.empty());

	if (f.size() == 1u) {
		return poly(1, ~f[0]);
	}

	auto temp = divide(mod, f);
	return -temp.first * ~temp.second;
}

decl(poly, /, f, g) {
	return f * ~g;
}

#define simplify(arg) std::cout << arg << " : " << fix(arg) << std::endl
#define inverse(arg) std::cout << '(' << arg << ")^-1 = " << ~arg << std::endl
#define calculate(lhs, op, rhs) std::cout << '(' << lhs << ") "#op" (" << rhs << ") = " << lhs op rhs << std::endl

int main() {
	mod = poly{ 3, 9, -6, 1 };
	std::cout << "mod : " << mod << std::endl;
	simplify((poly{ 0, 0, 0, 0, 1 }));
	simplify((poly{ 0, 0, 0, 0, 0, 1 }));
	simplify((poly{ 2, 0, 0, 0, -1, 3 }));
	inverse((poly{ 1, 1 }));
	inverse((poly{ 8, -6, 1 }));
	std::cout << std::endl << std::endl;
	mod = poly{ 2, 2, 0, 0, 0, 1 };
	std::cout << "mod : " << mod << std::endl;
	calculate((poly{ 2, 0, 1 }), *, (poly{ 0, 3, 0, 1 }));
	inverse((poly{ 0, 1 }));
	calculate((poly{ 0, 0, 0, 0, 1 }), *, (poly{ 5, 7, 3, 0, 1 }));
	calculate((poly{ 2, 1 }), /, (poly{ 3, 0, 1 }));
	return 0;
}
