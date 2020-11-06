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
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
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

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import java.util.List;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.ValidationErrorException;
import eu.internetofus.common.vertx.ModelsPageContext;
import eu.internetofus.common.vertx.QueryBuilder;
import eu.internetofus.common.vertx.Repository;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNormsPage;
import io.vertx.codegen.annotations.GenIgnore;
import io.vertx.codegen.annotations.ProxyGen;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.serviceproxy.ServiceBinder;

/**
 * The service to manage the {@link PublishedNorm} on the database.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ProxyGen
public interface NormsRepository {

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.persistence.norms";

  /**
   * Register this service.
   *
   * @param vertx   that contains the event bus to use.
   * @param pool    to create the database connections.
   * @param version of the schemas.
   *
   * @return the future that inform when the repository will be registered or not.
   */
  static Future<Void> register(final Vertx vertx, final MongoClient pool, final String version) {

    final var repository = new NormsRepositoryImpl(pool, version);
    new ServiceBinder(vertx).setAddress(NormsRepository.ADDRESS).register(NormsRepository.class, repository);
    return repository.migrateDocumentsToCurrentVersions();

  }

  /**
   * Create a proxy of the {@link NormsRepository}.
   *
   * @param vertx where the service has to be used.
   *
   * @return the norm.
   */
  static NormsRepository createProxy(final Vertx vertx) {

    return new NormsRepositoryVertxEBProxy(vertx, NormsRepository.ADDRESS);
  }

  /**
   * Search for the published norm with the specified identifier.
   *
   * @param id            identifier of the norm to search.
   * @param searchHandler handler to manage the search.
   */
  @GenIgnore
  default void searchPublishedNorm(final String id, final Handler<AsyncResult<PublishedNorm>> searchHandler) {

    this.searchPublishedNormObject(id, search -> {

      if (search.failed()) {

        searchHandler.handle(Future.failedFuture(search.cause()));

      } else {

        final var value = search.result();
        final var norm = Model.fromJsonObject(value, PublishedNorm.class);
        if (norm == null) {

          searchHandler.handle(Future.failedFuture("The stored published norm is not valid."));

        } else {

          searchHandler.handle(Future.succeededFuture(norm));
        }
      }
    });
  }

  /**
   * Search for the published norm with the specified identifier.
   *
   * @param id            identifier of the norm to search.
   * @param searchHandler handler to manage the search.
   */
  void searchPublishedNormObject(String id, Handler<AsyncResult<JsonObject>> searchHandler);

  /**
   * Store a published norm.
   *
   * @param norm         to store.
   * @param storeHandler handler to manage the store.
   */
  @GenIgnore
  default void storePublishedNorm(final PublishedNorm norm, final Handler<AsyncResult<PublishedNorm>> storeHandler) {

    final var object = norm.toJsonObject();
    if (object == null) {

      storeHandler.handle(Future.failedFuture("The published norm can not converted to JSON."));

    } else {

      this.storePublishedNorm(object, this.createHandlerMapJsonObjectToPublichedNorm(storeHandler));

    }
  }

  /**
   * Create a map to convert a {@link JsonObject} that responds an action to the respective norm.
   *
   * @param handler that will receive the result of the action.
   *
   * @return the handler that convert the {@link JsonObject} result
   *
   */
  private Handler<AsyncResult<JsonObject>> createHandlerMapJsonObjectToPublichedNorm(final Handler<AsyncResult<PublishedNorm>> handler) {

    return action -> {

      if (action.failed()) {

        handler.handle(Future.failedFuture(action.cause()));

      } else {

        final var value = action.result();
        final var storedNorm = Model.fromJsonObject(value, PublishedNorm.class);
        if (storedNorm == null) {

          handler.handle(Future.failedFuture("The stored published norm is not valid."));

        } else {

          handler.handle(Future.succeededFuture(storedNorm));
        }

      }

    };
  }

  /**
   * Store a published norm.
   *
   * @param norm         to store.
   * @param storeHandler handler to manage the search.
   */
  void storePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> storeHandler);

  /**
   * Update a published norm.
   *
   * @param norm          to update.
   * @param updateHandler handler to manage the update.
   */
  @GenIgnore
  default void updatePublishedNorm(final PublishedNorm norm, final Handler<AsyncResult<Void>> updateHandler) {

    final var object = norm.toJsonObject();
    if (object == null) {

      updateHandler.handle(Future.failedFuture("The published norm can not converted to JSON."));

    } else {

      this.updatePublishedNorm(object, updateHandler);

    }
  }

  /**
   * Update a published norm.
   *
   * @param norm          to update.
   * @param updateHandler handler to manage the update result.
   */
  void updatePublishedNorm(JsonObject norm, Handler<AsyncResult<Void>> updateHandler);

  /**
   * Delete a published norm.
   *
   * @param id            identifier of the norm to delete.
   * @param deleteHandler handler to manage the delete result.
   */
  void deletePublishedNorm(String id, Handler<AsyncResult<Void>> deleteHandler);

  /**
   * Create a query to obtain the norms that has the specified parameters.
   *
   * @param name        of the norms to return.
   * @param description of the norms to return.
   * @param keywords    of the norms to return.
   * @param publisherId identifier of the user that has published the norm.
   * @param publishFrom minimal deadline time stamp of the tasks to return.
   * @param publishTo   maximal deadline time stamp of the tasks to return.
   *
   * @return the query that will return the required norms.
   */
  static JsonObject createPublishedNormsPageQuery(final String name, final String description, final List<String> keywords, final String publisherId, final Long publishFrom, final Long publishTo) {

    return new QueryBuilder().withEqOrRegex("name", name).withEqOrRegex("description", description).withEqOrRegex("keywords", keywords).withEqOrRegex("publisherId", publisherId).withRange("_lastUpdateTs", publishFrom, publishTo).build();
  }

  /**
   * Create the components used to sort.
   *
   * @param order to use.
   *
   * @return the component that has to be used to sort the norms.
   *
   * @throws ValidationErrorException if the sort parameter is not valid.
   */
  static JsonObject createPublishedNormsPageSort(final List<String> order) throws ValidationErrorException {

    final var sort = Repository.queryParamToSort(order, "bad_order", (value) -> {

      switch (value) {
      case "name":
      case "description":
      case "keywords":
      case "publisherId":
        return value;
      case "publish":
      case "publishTime":
      case "publishedTime":
        return "_lastUpdateTs";
      default:
        return null;
      }

    });
    return sort;
  }

  /**
   * Retrieve the norms defined on the context.
   *
   * @param context that describe witch page want to obtain.
   * @param handler for the obtained page.
   */
  @GenIgnore
  default void retrievePublishedNormsPageObject(final ModelsPageContext context, final Handler<AsyncResult<JsonObject>> handler) {

    this.retrievePublishedNormsPageObject(context.query, context.sort, context.offset, context.limit, handler);
  }

  /**
   * Retrieve the norms defined on the context.
   *
   * @param context       that describe witch page want to obtain.
   * @param searchHandler for the obtained page.
   */
  @GenIgnore
  default void retrievePublishedNormsPage(final ModelsPageContext context, final Handler<AsyncResult<PublishedNormsPage>> searchHandler) {

    this.retrievePublishedNormsPage(context.query, context.sort, context.offset, context.limit, searchHandler);

  }

  /**
   * Retrieve the norms defined on the context.
   *
   * @param query         to obtain the required norms.
   * @param sort          describe how has to be ordered the obtained norms.
   * @param offset        the index of the first community to return.
   * @param limit         the number maximum of norms to return.
   * @param searchHandler for the obtained page.
   */
  @GenIgnore
  default void retrievePublishedNormsPage(final JsonObject query, final JsonObject sort, final int offset, final int limit, final Handler<AsyncResult<PublishedNormsPage>> searchHandler) {

    this.retrievePublishedNormsPageObject(query, sort, offset, limit, search -> {

      if (search.failed()) {

        searchHandler.handle(Future.failedFuture(search.cause()));

      } else {

        final var value = search.result();
        final var page = Model.fromJsonObject(value, PublishedNormsPage.class);
        if (page == null) {

          searchHandler.handle(Future.failedFuture("The stored norms page is not valid."));

        } else {

          searchHandler.handle(Future.succeededFuture(page));
        }
      }
    });

  }

  /**
   * Retrieve a page with some norms.
   *
   * @param query   to obtain the required norms.
   * @param sort    describe how has to be ordered the obtained norms.
   * @param offset  the index of the first community to return.
   * @param limit   the number maximum of norms to return.
   * @param handler to inform of the found norms.
   */
  void retrievePublishedNormsPageObject(JsonObject query, JsonObject sort, int offset, int limit, Handler<AsyncResult<JsonObject>> handler);

}
