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

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.ValidationErrorException;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.vertx.OperationReponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
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
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * The repository to manage the norms.
   */
  protected NormsRepository repository;

  /**
   * The manager to manage the users profile.
   */
  protected WeNetProfileManager profileManager;

  /**
   * Create a new instance to provide the services of the {@link Norms}.
   *
   * @param vertx where resource is defined.
   */
  public NormsResource(final Vertx vertx) {

    this.vertx = vertx;
    this.repository = NormsRepository.createProxy(vertx);
    this.profileManager = WeNetProfileManager.createProxy(vertx);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void publishNorm(final JsonObject body, final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    final var publishedNorm = Model.fromJsonObject(body, PublishedNorm.class);
    if (publishedNorm == null) {

      Logger.debug("The {} is not a valid norm to publish.", body);
      OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_publishedNorm", "The norm to publish is not not right.");

    } else {

      publishedNorm.validate("bad_publishedNorm", this.vertx).onComplete(validation -> {

        if (validation.failed()) {

          final var cause = validation.cause();
          Logger.debug(cause, "The {} is not valid.", publishedNorm);
          OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

        } else {

          this.repository.storePublishedNorm(publishedNorm, stored -> {
            if (stored.failed()) {

              final var cause = stored.cause();
              Logger.debug(cause, "Cannot store  {}.", publishedNorm);
              OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

            } else {

              OperationReponseHandlers.responseWith(resultHandler, Status.CREATED, stored.result());
            }

          });
        }

      });
    }

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrievePublishedNormsPage(final String name, final String description, final List<String> keywords, final String publisherId, final Long publishFrom, final Long publishTo, final List<String> order, final int offset,
      final int limit, final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    final var query = NormsRepository.createPublishedNormsPageQuery(name, description, keywords, publisherId, publishFrom, publishTo);

    try {

      final var sort = NormsRepository.createPublishedNormsPageSort(order);
      this.repository.retrievePublishedNormsPageObject(query, sort, offset, limit, search -> {

        if (search.failed()) {

          final var cause = search.cause();
          Logger.debug(cause, "Cannot found published norms.");
          OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

        } else {

          final var page = search.result();
          OperationReponseHandlers.responseOk(resultHandler, page);
        }
      });

    } catch (final ValidationErrorException error) {

      Logger.debug(error, "GET /Norms with {} order by {}=> Retrieve error", query, order);
      OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, error);

    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrievePublishedNorm(final String publishedNormId, final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    this.repository.searchPublishedNormObject(publishedNormId, search -> {

      final var publishedNorm = search.result();
      if (publishedNorm == null) {

        Logger.debug(search.cause(), "Not found published norm for {}", publishedNormId);
        OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_FOUND, "not_found_published_norm", "Does not exist a published norm associated to '" + publishedNormId + "'.");

      } else {

        OperationReponseHandlers.responseOk(resultHandler, publishedNorm);

      }
    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updatePublishedNorm(final String publishedNormId, final JsonObject body, final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    final var source = Model.fromJsonObject(body, PublishedNorm.class);
    if (source == null) {

      Logger.debug("The {} is not a valid published norm to update.", body);
      OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_published_norm_to_update", "The published norm to update is not right.");

    } else {

      this.repository.searchPublishedNorm(publishedNormId, search -> {

        final var target = search.result();
        if (target == null) {

          Logger.debug(search.cause(), "Not found published norm {} to update", publishedNormId);
          OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_FOUND, "not_found_published_norm_to_update", "You can not update the published norm '" + publishedNormId + "', because it does not exist.");

        } else {

          source.id = null;
          if (source.norm != null) {

            source.norm.id = null;
          }
          source.validate("bad_published_norm", this.vertx).onComplete(validate -> {

            if (validate.failed()) {

              final var cause = validate.cause();
              Logger.debug(cause, "Cannot update  {} with {}.", target, source);
              OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

            } else {

              source.id = target.id;
              source._creationTs = target._creationTs;
              source._lastUpdateTs = TimeManager.now();
              source.norm.id = target.norm.id;
              this.repository.updatePublishedNorm(source, updated -> {

                if (updated.failed()) {

                  final var cause = updated.cause();
                  Logger.debug(cause, "Cannot update  {}.", target);
                  OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

                } else {

                  OperationReponseHandlers.responseOk(resultHandler, source);

                }

              });
            }
          });
        }
      });
    }

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deletePublishedNorm(final String publishedNormId, final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    this.repository.deletePublishedNorm(publishedNormId, delete -> {

      if (delete.failed()) {

        final var cause = delete.cause();
        Logger.debug(cause, "Cannot delete the published norm  {}.", publishedNormId);
        OperationReponseHandlers.responseFailedWith(resultHandler, Status.NOT_FOUND, cause);

      } else {

        OperationReponseHandlers.responseOk(resultHandler);
      }

    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergePublishedNorm(final String publishedNormId, final JsonObject body, final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    final var source = Model.fromJsonObject(body, PublishedNorm.class);
    if (source == null) {

      Logger.debug("The {} is not a valid published norm to merge.", body);
      OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_published_norm_to_merge", "The published norm to merge is not right.");

    } else {

      this.repository.searchPublishedNorm(publishedNormId, search -> {

        final var target = search.result();
        if (target == null) {

          Logger.debug(search.cause(), "Not found published norm {} to merge", publishedNormId);
          OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_FOUND, "not_found_published_norm_to_merge", "You can not merge the published norm '" + publishedNormId + "', because it does not exist.");

        } else {

          target.merge(source, "bad_publishedNorm", this.vertx).onComplete(merge -> {

            if (merge.failed()) {

              final var cause = merge.cause();
              Logger.debug(cause, "Cannot merge  {} with {}.", target, source);
              OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

            } else {

              final var merged = merge.result();
              if (merged.equals(target)) {

                OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "published_norm_to_merge_equal_to_original",
                    "You can not merge the published norm '" + publishedNormId + "', because the new values is equals to the current one.");

              } else {

                merged._lastUpdateTs = TimeManager.now();
                this.repository.updatePublishedNorm(merged, updated -> {

                  if (updated.failed()) {

                    final var cause = updated.cause();
                    Logger.debug(cause, "Cannot merge  {}.", target);
                    OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

                  } else {

                    OperationReponseHandlers.responseOk(resultHandler, merged);

                  }

                });
              }
            }
          });

        }
      });
    }

  }

}
