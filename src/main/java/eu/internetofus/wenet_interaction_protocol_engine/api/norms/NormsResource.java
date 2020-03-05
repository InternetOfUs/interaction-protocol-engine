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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import java.util.List;

import javax.ws.rs.core.Response.Status;

import org.tinylog.Logger;

import eu.internetofus.wenet_interaction_protocol_engine.api.OperationReponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.api.Operations;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;

/**
 * Implements the services defined in the {@link Norms}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class NormsResource implements Norms {

	/**
	 * The repository to manage the norms.
	 */
	protected NormsRepository repository;

	/**
	 * The manager to manage the users profile.
	 */
	protected WeNetProfileManagerService profileManager;

	/**
	 * Create an empty resource. This is only used for unit tests.
	 */
	protected NormsResource() {

	}

	/**
	 * Create a new instance to provide the services of the {@link Norms}.
	 *
	 * @param vertx where resource is defined.
	 */
	public NormsResource(Vertx vertx) {

		this.repository = NormsRepository.createProxy(vertx);
		this.profileManager = WeNetProfileManagerService.createProxy(vertx);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void publishNorm(JsonObject body, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_IMPLEMENTED, "to_do",
				"Not implemented yet");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void retrievePublishedNormsPage(OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		final JsonObject params = Operations.getQueryParamters(context);
		final int offset = params.getInteger("offset", 0);
		final int limit = params.getInteger("limit", 10);
		final String name = params.getString("name", null);
		final String description = params.getString("description", null);
		final List<String> keywords = Operations.toListString(params.getJsonArray("keyword", null));
		final String publisherId = params.getString("publisherId", null);
		final Long publishFrom = params.getLong("publishFrom", null);
		final Long publishTo = params.getLong("publishTo", null);

		this.repository.searchPublishedNormsPageObject(name, description, keywords, publisherId, publishFrom, publishTo,
				offset, limit, search -> {

					if (search.failed()) {

						final Throwable cause = search.cause();
						Logger.debug(cause, "Cannot found published norms.");
						OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

					} else {

						final JsonObject page = search.result();
						OperationReponseHandlers.responseOk(resultHandler, page);
					}
				});
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void retrievePublishedNorm(String publishedNormId, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		this.repository.searchPublishedNormObject(publishedNormId, search -> {

			final JsonObject publishedNorm = search.result();
			if (publishedNorm == null) {

				Logger.debug(search.cause(), "Not found published norm for {}", publishedNormId);
				OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_FOUND, "not_found_published_norm",
						"Does not exist a published norm associated to '" + publishedNormId + "'.");

			} else {

				OperationReponseHandlers.responseOk(resultHandler, publishedNorm);

			}
		});

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updatePublishedNorm(String publishedNormId, JsonObject body, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_IMPLEMENTED, "to_do",
				"Not implemented yet");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void deletePublishedNorm(String publishedNormId, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		this.repository.deletePublishedNorm(publishedNormId, delete -> {

			if (delete.failed()) {

				final Throwable cause = delete.cause();
				Logger.debug(cause, "Cannot delete the published norm  {}.", publishedNormId);
				OperationReponseHandlers.responseFailedWith(resultHandler, Status.NOT_FOUND, cause);

			} else {

				OperationReponseHandlers.responseOk(resultHandler);
			}

		});

	}

}
