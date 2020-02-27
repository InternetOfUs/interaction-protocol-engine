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

package eu.internetofus.wenet_interaction_protocol_engine.services;

import javax.ws.rs.core.Response.Status;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;

/**
 * A component that provide the interaction over .
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class Service {

	/**
	 * The pool of web clients.
	 */
	protected WebClient client;

	/**
	 * The port of the service.
	 */
	protected int port;

	/**
	 * The host of the service.
	 */
	protected String host;

	/**
	 * The API path of the service.
	 */
	protected String apiPath;

	/**
	 * Create a new service.
	 *
	 * @param client to interact with the other modules.
	 * @param conf   configuration.
	 */
	public Service(WebClient client, JsonObject conf) {

		this.client = client;
		this.port = conf.getInteger("port", 80);
		this.host = conf.getString("host", "localhost");
		this.apiPath = conf.getString("apiPath", "/api");

	}

	/**
	 * Post a resource.
	 *
	 * @param path        to the resource to post.
	 * @param content     resource to post.
	 * @param postHandler the handler to manager the posted resource.
	 *
	 */
	protected void post(String path, JsonObject content, Handler<AsyncResult<JsonObject>> postHandler) {

		final String requestURI = this.apiPath + path;
		this.client.post(this.port, this.host, requestURI).sendJson(content, post -> {

			if (post.failed()) {

				postHandler.handle(Future.failedFuture(post.cause()));

			} else {

				try {

					final HttpResponse<Buffer> result = post.result();
					if (Status.Family.familyOf(result.statusCode()) == Status.Family.SUCCESSFUL) {

						final JsonObject createdProfile = result.bodyAsJsonObject();
						postHandler.handle(Future.succeededFuture(createdProfile));

					} else {

						postHandler.handle(Future.failedFuture(result.statusMessage()));
					}

				} catch (final Throwable cause) {
					// cannot obtain the received profile
					postHandler.handle(Future.failedFuture(cause));
				}

			}
		});

	}

	/**
	 * Get a resource.
	 *
	 * @param path       of the resource to get.
	 * @param getHandler the handler to manage the receiver resource.
	 */
	protected void get(String path, Handler<AsyncResult<JsonObject>> getHandler) {

		final String requestURI = this.apiPath + path;
		this.client.get(this.port, this.host, requestURI).send(get -> {

			if (get.failed()) {

				getHandler.handle(Future.failedFuture(get.cause()));

			} else {

				final HttpResponse<Buffer> result = get.result();
				if (Status.Family.familyOf(result.statusCode()) == Status.Family.SUCCESSFUL) {

					final JsonObject createdProfile = result.bodyAsJsonObject();
					getHandler.handle(Future.succeededFuture(createdProfile));

				} else {

					getHandler.handle(Future.failedFuture(result.statusMessage()));
				}
			}
		});

	}

	/**
	 * Delete a resource.
	 *
	 * @param path          of the resource to delete.
	 * @param deleteHandler the handler to manage the receiver resource.
	 */
	protected void delete(String path, Handler<AsyncResult<Void>> deleteHandler) {

		final String requestURI = this.apiPath + path;
		this.client.delete(this.port, this.host, requestURI).send(delete -> {

			if (delete.failed()) {

				deleteHandler.handle(Future.failedFuture(delete.cause()));

			} else {

				final HttpResponse<Buffer> result = delete.result();
				if (Status.Family.familyOf(result.statusCode()) == Status.Family.SUCCESSFUL) {

					deleteHandler.handle(Future.succeededFuture());

				} else {

					deleteHandler.handle(Future.failedFuture(result.statusMessage()));
				}
			}
		});

	}

}
