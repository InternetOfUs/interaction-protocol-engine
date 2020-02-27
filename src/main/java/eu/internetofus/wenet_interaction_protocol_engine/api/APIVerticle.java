/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api;

import javax.ws.rs.core.Response.Status;

import org.tinylog.Logger;

import eu.internetofus.wenet_interaction_protocol_engine.api.versions.Versions;
import eu.internetofus.wenet_interaction_protocol_engine.api.versions.VersionsResource;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.api.contract.openapi3.OpenAPI3RouterFactory;
import io.vertx.serviceproxy.ServiceBinder;

/**
 * The verticle that provide the API management.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class APIVerticle extends AbstractVerticle {

	/**
	 * The server that manage the HTTP requests.
	 */
	protected HttpServer server;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void start(Promise<Void> startPromise) throws Exception {

		OpenAPI3RouterFactory.create(this.vertx, "wenet-interaction-protocol-engine-api.yaml", createRouterFactory -> {
			if (createRouterFactory.succeeded()) {

				try {

					final OpenAPI3RouterFactory routerFactory = createRouterFactory.result();

					routerFactory.mountServiceInterface(Versions.class, Versions.ADDRESS);
					new ServiceBinder(this.vertx).setAddress(Versions.ADDRESS).register(Versions.class,
							new VersionsResource(this));

					// bind the ERROR handlers
					final Router router = routerFactory.getRouter();
					router.errorHandler(Status.NOT_FOUND.getStatusCode(), NotFoundHandler.build());
					router.errorHandler(Status.BAD_REQUEST.getStatusCode(), BadRequestHandler.build());

					final JsonObject apiConf = this.config().getJsonObject("api", new JsonObject());
					final HttpServerOptions httpServerOptions = new HttpServerOptions(apiConf);
					this.server = this.vertx.createHttpServer(httpServerOptions);
					this.server.requestHandler(router).listen(startServer -> {
						if (startServer.failed()) {

							startPromise.fail(startServer.cause());

						} else {

							final HttpServer httpServer = startServer.result();
							final String host = httpServerOptions.getHost();
							final int actualPort = httpServer.actualPort();
							apiConf.put("port", actualPort);
							Logger.info("The server is ready at http://{}:{}", host, actualPort);
							startPromise.complete();
						}
					});

				} catch (final Throwable throwable) {
					// Can not start the server , may be the configuration is wrong
					startPromise.fail(throwable);
				}

			} else {
				// In theory never happens, Only can happens if specification is not right or
				// not present on the path
				startPromise.fail(createRouterFactory.cause());
			}
		});

	}

	/**
	 * Stop the HTTP server.
	 *
	 * {@inheritDoc}
	 *
	 * @see #server
	 */
	@Override
	public void stop() {

		if (this.server != null) {

			this.server.close();
			this.server = null;
		}
	}

}