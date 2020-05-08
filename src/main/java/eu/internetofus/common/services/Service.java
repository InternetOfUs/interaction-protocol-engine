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

package eu.internetofus.common.services;

import javax.ws.rs.core.Response.Status;

import eu.internetofus.common.api.models.ErrorException;
import eu.internetofus.common.api.models.ErrorMessage;
import eu.internetofus.common.api.models.Model;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;

/**
 * A component that provide the interaction over other WeNet components.
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
	 * This is {@code true} if has to use an SSL connection.
	 */
	protected boolean ssl;

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
		this.port = conf.getInteger("port", 8080);
		this.host = conf.getString("host", "localhost");
		this.ssl = conf.getBoolean("ssl", false);
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
		this.client.post(this.port, this.host, requestURI).ssl(this.ssl).sendJson(content,
				this.responseHandler(postHandler));

	}

	/**
	 * Put a resource.
	 *
	 * @param path       to the resource to put.
	 * @param content    resource to put.
	 * @param putHandler the handler to manager the puted resource.
	 *
	 */
	protected void put(String path, JsonObject content, Handler<AsyncResult<JsonObject>> putHandler) {

		final String requestURI = this.apiPath + path;
		this.client.put(this.port, this.host, requestURI).ssl(this.ssl).sendJson(content, this.responseHandler(putHandler));

	}

	/**
	 * Create a handler to manage the response of an HTTP request.
	 *
	 * @param actionHandler handler to manage the action result.
	 *
	 * @return the handler to manage the answer got an HTTP action.
	 */
	protected Handler<AsyncResult<HttpResponse<Buffer>>> responseHandler(Handler<AsyncResult<JsonObject>> actionHandler) {

		return action -> {

			try {

				if (action.failed()) {

					actionHandler.handle(Future.failedFuture(action.cause()));

				} else {

					final HttpResponse<Buffer> result = action.result();
					if (Status.Family.familyOf(result.statusCode()) == Status.Family.SUCCESSFUL) {

						if (result.statusCode() == Status.NO_CONTENT.getStatusCode()) {

							actionHandler.handle(Future.succeededFuture());

						} else {

							final JsonObject body = result.bodyAsJsonObject();
							actionHandler.handle(Future.succeededFuture(body));
						}

					} else {

						final ErrorMessage errorMessage = Model.fromBuffer(result.body(), ErrorMessage.class);
						if (errorMessage == null) {

							actionHandler.handle(Future.failedFuture(result.statusMessage()));

						} else {

							actionHandler.handle(Future.failedFuture(new ErrorException(errorMessage)));
						}
					}

				}

			} catch (final Throwable cause) {
				// cannot obtain the received profile
				actionHandler.handle(Future.failedFuture(cause));
			}

		};
	}

	/**
	 * Get a resource.
	 *
	 * @param path       of the resource to get.
	 * @param getHandler the handler to manage the receiver resource.
	 */
	protected void get(String path, Handler<AsyncResult<JsonObject>> getHandler) {

		final String requestURI = this.apiPath + path;
		this.client.get(this.port, this.host, requestURI).ssl(this.ssl).send(this.responseHandler(getHandler));

	}

	/**
	 * Delete a resource.
	 *
	 * @param path          of the resource to delete.
	 * @param deleteHandler the handler to manage the receiver resource.
	 */
	protected void delete(String path, Handler<AsyncResult<JsonObject>> deleteHandler) {

		final String requestURI = this.apiPath + path;
		this.client.delete(this.port, this.host, requestURI).ssl(this.ssl).send(this.responseHandler(deleteHandler));

	}

	/**
	 * Create a handler to convert a {@link JsonObject} to a {@link Model}
	 *
	 * @param type            of model to handle.
	 * @param retrieveHandler handler to inform of the model obtained by the
	 *                        {@link JsonObject}.
	 *
	 * @return a handler to process the {@link JsonObject} and return the model
	 */
	public static <T extends Model> Handler<AsyncResult<JsonObject>> handlerForModel(Class<T> type,
			Handler<AsyncResult<T>> retrieveHandler) {

		return handler -> {

			if (handler.failed()) {

				retrieveHandler.handle(Future.failedFuture(handler.cause()));

			} else {

				final JsonObject result = handler.result();
				if (result == null) {

					retrieveHandler.handle(Future.succeededFuture());

				} else {

					final T model = Model.fromJsonObject(result, type);
					if (model == null) {

						retrieveHandler.handle(Future.failedFuture(result + " is not of the type '" + type + "'."));

					} else {

						retrieveHandler.handle(Future.succeededFuture(model));
					}

				}
			}

		};
	}

}
